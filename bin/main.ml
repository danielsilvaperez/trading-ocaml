open Cmdliner

let load_config path =
  match Trading_core.Config.load path with
  | Ok c -> c
  | Error (Trading_core.Types.Config_error msg) ->
      prerr_endline ("Failed to load config, using defaults: " ^ msg);
      Trading_core.Config.default
  | Error _ -> Trading_core.Config.default

let instrument_of_symbol symbol =
  let asset_class =
    if String.contains symbol '-' then Trading_core.Types.Crypto else Trading_core.Types.Equity
  in
  Trading_core.Types.{ symbol; asset_class; venue_hint = None }

let pp_status s =
  Printf.printf "running=%b mode=%s enabled=%d breaker=%b\n%!" s.Trading_engine.Events.running
    (match s.mode with
    | Trading_core.Config.Live -> "live"
    | Trading_core.Config.Paper -> "paper"
    | Trading_core.Config.Backtest -> "backtest"
    | Trading_core.Config.Simulation -> "simulation")
    (List.length s.enabled_strategies) s.breaker_tripped

let run_engine config_path web telegram port =
  let cfg = load_config config_path in
  let cfg = { cfg with interfaces = { cfg.interfaces with web_port = port } } in
  let engine = Trading_engine.Engine.create cfg in
  let tasks = ref [ Trading_engine.Engine.run engine ] in
  if web then tasks := Trading_interfaces.Web_dashboard.run ~engine ~port :: !tasks;
  if telegram then tasks := Trading_interfaces.Telegram_bot.run_repl ~engine :: !tasks;
  Lwt_main.run (Lwt.join !tasks);
  `Ok ()

let status config_path =
  let engine = Trading_engine.Engine.create (load_config config_path) in
  pp_status (Trading_engine.Engine.status engine);
  `Ok ()

let portfolio config_path =
  let engine = Trading_engine.Engine.create (load_config config_path) in
  let s = Trading_engine.Engine.portfolio_snapshot engine in
  Printf.printf "equity=%.2f realized=%.2f unrealized=%.2f drawdown=%.2f%%\n%!" s.portfolio.equity
    s.realized_pnl s.unrealized_pnl (s.drawdown *. 100.);
  `Ok ()

let strategy_toggle config_path id enabled =
  let engine = Trading_engine.Engine.create (load_config config_path) in
  if enabled then Trading_engine.Engine.enable_strategy engine id
  else Trading_engine.Engine.disable_strategy engine id;
  pp_status (Trading_engine.Engine.status engine);
  `Ok ()

let backtest config_path csv strategy symbol from_date to_date =
  let cfg = load_config config_path in
  let instrument = instrument_of_symbol symbol in
  let cfg =
    {
      cfg with
      strategies =
        [
          {
            Trading_core.Config.id = strategy ^ "::" ^ symbol;
            name = strategy;
            enabled = true;
            weight = 1.0;
            instruments = [ instrument ];
            params = [ ("fast", 20.); ("slow", 50.) ];
          };
        ];
    }
  in
  ignore from_date;
  ignore to_date;
  match
    Trading_backtest.Backtest_engine.run ~config:cfg ~csv_path:csv ~instrument
      ~execution_model:{ base_slippage_bps = 2.; commission_bps = 1.; latency_ms = 5 }
  with
  | Error e ->
      (match e with
      | Trading_core.Types.Config_error msg
      | Trading_core.Types.Invalid_input msg
      | Trading_core.Types.Strategy_error msg
      | Trading_core.Types.Internal_error msg
      | Trading_core.Types.Venue_error msg -> prerr_endline msg
      | Trading_core.Types.Risk_error rv ->
          Format.eprintf "%a@." Trading_core.Types.pp_risk_violation rv);
      `Error (false, "backtest failed")
  | Ok s ->
      Printf.printf
        "Backtest Summary\nstart_equity=%.2f\nend_equity=%.2f\nreturn=%.2f%%\nmax_drawdown=%.2f%%\nsharpe=%.3f\nsortino=%.3f\ntrades=%d\n%!"
        s.start_equity s.end_equity (s.total_return *. 100.) (s.max_drawdown *. 100.) s.sharpe s.sortino
        s.trades;
      `Ok ()

let simulate paths horizon ruin_threshold =
  let base_returns =
    [ 0.001; -0.002; 0.0005; 0.003; -0.0015; 0.0025; -0.0007; 0.0018; 0.0009; -0.003 ]
  in
  let dist =
    Trading_simulation.Monte_carlo.run
      ~config:
        {
          paths;
          horizon;
          ruin_threshold;
          slippage_bps_range = (0.5, 3.0);
          perturbation_std = 0.002;
        }
      ~source_returns:base_returns
  in
  let mean xs =
    match xs with [] -> 0. | _ -> List.fold_left ( +. ) 0. xs /. float_of_int (List.length xs)
  in
  Printf.printf
    "Monte Carlo\npaths=%d\nhorizon=%d\nmean_return=%.2f%%\nmean_drawdown=%.2f%%\nprobability_of_ruin=%.2f%%\nexpected_value=%.2f%%\n%!"
    paths horizon (mean dist.returns *. 100.) (mean dist.drawdowns *. 100.)
    (dist.probability_of_ruin *. 100.) (dist.expected_value *. 100.);
  `Ok ()

let config_arg =
  let doc = "Path to YAML-like config file." in
  Arg.(value & opt string "config.yaml" & info [ "c"; "config" ] ~docv:"PATH" ~doc)

let run_cmd =
  let web = Arg.(value & flag & info [ "web" ] ~doc:"Run web dashboard") in
  let telegram = Arg.(value & flag & info [ "telegram" ] ~doc:"Run Telegram REPL bridge") in
  let port = Arg.(value & opt int 8080 & info [ "port" ] ~doc:"Web dashboard port") in
  let term = Term.(ret (const run_engine $ config_arg $ web $ telegram $ port)) in
  Cmd.v (Cmd.info "run" ~doc:"Run trading engine") term

let status_cmd =
  let term = Term.(ret (const status $ config_arg)) in
  Cmd.v (Cmd.info "status" ~doc:"Engine status") term

let portfolio_cmd =
  let term = Term.(ret (const portfolio $ config_arg)) in
  Cmd.v (Cmd.info "portfolio" ~doc:"Portfolio snapshot") term

let backtest_cmd =
  let csv = Arg.(value & opt string "data/BTC-USD.csv" & info [ "csv" ] ~doc:"Historical CSV") in
  let strategy = Arg.(value & opt string "sma" & info [ "strategy" ] ~doc:"Strategy name") in
  let symbol = Arg.(value & opt string "BTC-USD" & info [ "symbol" ] ~doc:"Instrument symbol") in
  let from_date = Arg.(value & opt string "2022-01-01" & info [ "from" ]) in
  let to_date = Arg.(value & opt string "2023-01-01" & info [ "to" ]) in
  let term =
    Term.(ret (const backtest $ config_arg $ csv $ strategy $ symbol $ from_date $ to_date))
  in
  Cmd.v (Cmd.info "backtest" ~doc:"Run deterministic backtest") term

let simulate_cmd =
  let paths = Arg.(value & opt int 1000 & info [ "paths" ] ~doc:"Monte Carlo paths") in
  let horizon = Arg.(value & opt int 252 & info [ "horizon" ] ~doc:"Path horizon") in
  let ruin =
    Arg.(value & opt float 0.70 & info [ "ruin-threshold" ] ~doc:"Ruin threshold on NAV")
  in
  let term = Term.(ret (const simulate $ paths $ horizon $ ruin)) in
  Cmd.v (Cmd.info "simulate" ~doc:"Run Monte Carlo simulation") term

let strategy_enable_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"STRATEGY_ID") in
  let term = Term.(ret (const strategy_toggle $ config_arg $ id $ const true)) in
  Cmd.v (Cmd.info "enable" ~doc:"Enable strategy") term

let strategy_disable_cmd =
  let id = Arg.(required & pos 0 (some string) None & info [] ~docv:"STRATEGY_ID") in
  let term = Term.(ret (const strategy_toggle $ config_arg $ id $ const false)) in
  Cmd.v (Cmd.info "disable" ~doc:"Disable strategy") term

let strategy_cmd =
  Cmd.group (Cmd.info "strategy" ~doc:"Strategy controls")
    [ strategy_enable_cmd; strategy_disable_cmd ]

let default_cmd =
  Cmd.group (Cmd.info "trading-ocaml" ~doc:"Institutional-style OCaml trading engine")
    [ run_cmd; status_cmd; portfolio_cmd; backtest_cmd; simulate_cmd; strategy_cmd ]

let () = exit (Cmd.eval default_cmd)
