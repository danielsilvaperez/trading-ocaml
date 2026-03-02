open Trading_core

let parse_command line =
  let parts = line |> String.trim |> String.split_on_char ' ' in
  match parts with
  | [ "/status" ] -> Some Trading_engine.Events.Get_status
  | [ "/positions" ] | [ "/pnl" ] | [ "/risk" ] | [ "/portfolio" ] ->
      Some Trading_engine.Events.Get_portfolio
  | [ "/enable"; strategy ] -> Some (Trading_engine.Events.Enable_strategy strategy)
  | [ "/disable"; strategy ] -> Some (Trading_engine.Events.Disable_strategy strategy)
  | [ "/kill" ] -> Some Trading_engine.Events.Trigger_kill_switch
  | _ -> None

let print_status engine =
  let s = Trading_engine.Engine.status engine in
  Printf.printf
    "running=%b mode=%s enabled=%d breaker=%b\n%!" s.running
    (match s.mode with
    | Config.Live -> "live"
    | Config.Paper -> "paper"
    | Config.Backtest -> "backtest"
    | Config.Simulation -> "simulation")
    (List.length s.enabled_strategies) s.breaker_tripped

let print_portfolio engine =
  let p = Trading_engine.Engine.portfolio_snapshot engine in
  Printf.printf "equity=%.2f realized=%.2f unrealized=%.2f drawdown=%.2f%%\n%!"
    p.portfolio.equity p.realized_pnl p.unrealized_pnl (p.drawdown *. 100.)

let run_repl ~engine =
  let bus = Trading_engine.Engine.bus engine in
  let rec loop () =
    let%lwt line = Lwt_io.(read_line_opt stdin) in
    match line with
    | None -> Lwt.return_unit
    | Some line ->
        let%lwt () =
          match parse_command line with
          | Some cmd ->
              let%lwt () =
                Trading_core.Message_bus.publish bus (Trading_engine.Events.Control cmd)
              in
              (match cmd with
              | Trading_engine.Events.Get_status -> print_status engine
              | Trading_engine.Events.Get_portfolio -> print_portfolio engine
              | Trading_engine.Events.Enable_strategy _
              | Trading_engine.Events.Disable_strategy _
              | Trading_engine.Events.Replace_strategy _
              | Trading_engine.Events.Trigger_kill_switch -> print_endline "ok");
              Lwt.return_unit
          | None ->
              print_endline
                "supported: /status /positions /enable <id> /disable <id> /pnl /risk /kill";
              Lwt.return_unit
        in
        loop ()
  in
  loop ()
