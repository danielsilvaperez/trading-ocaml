open Trading_core

type t = {
  cfg : Config.t;
  strategy_mgr : Strategy_manager.t;
  risk_mgr : Trading_risk.Manager.t;
  portfolio : Trading_portfolio.Portfolio_state.t;
  order_router : Order_router.t;
  control_bus : Events.bus_message Message_bus.t;
  mutable running : bool;
  policy : Allocation.policy;
}

let policy_from_cfg (_cfg : Config.t) = Allocation.EnsembleVoting

let create (cfg : Config.t) =
  Random.self_init ();
  Observability.init_logging ();
  let strategy_mgr =
    match Strategy_manager.create cfg.strategies with
    | Ok m -> m
    | Error (Types.Strategy_error msg) -> failwith msg
    | Error _ -> failwith "strategy initialization failed"
  in
  let risk_mgr = Trading_risk.Manager.create cfg.risk in
  let portfolio = Trading_portfolio.Portfolio_state.create ~capital:cfg.initial_capital in
  let observability = Observability.create () in
  let default_venue =
    match cfg.mode with
    | Config.Live -> Types.Coinbase
    | Config.Paper -> Types.Paper
    | Config.Backtest -> Types.Backtest
    | Config.Simulation -> Types.Simulation
  in
  let order_router =
    Order_router.create
      Order_router.
        {
          risk = risk_mgr;
          portfolio;
          observability;
          default_venue;
        }
  in
  {
    cfg;
    strategy_mgr;
    risk_mgr;
    portfolio;
    order_router;
    control_bus = Message_bus.create ();
    running = false;
    policy = policy_from_cfg cfg;
  }

let status t =
  {
    Events.mode = t.cfg.mode;
    running = t.running;
    enabled_strategies = Strategy_manager.enabled_ids t.strategy_mgr;
    breaker_tripped = Trading_risk.Manager.circuit_breaker_tripped t.risk_mgr;
  }

let enable_strategy t id = Strategy_manager.set_enabled t.strategy_mgr ~id true
let disable_strategy t id = Strategy_manager.set_enabled t.strategy_mgr ~id false
let portfolio_snapshot t = Trading_portfolio.Portfolio_state.snapshot t.portfolio
let bus t = t.control_bus

let process_signal t signal =
  let snap = Trading_portfolio.Portfolio_state.snapshot t.portfolio in
  let daily_drawdown = snap.drawdown in
  let intraday_pnl = snap.realized_pnl +. snap.unrealized_pnl in
  let%lwt routed =
    Order_router.route_signal t.order_router ~daily_drawdown ~intraday_pnl ~instrument_volatility:None signal
  in
  match routed with
  | Ok (Types.Filled qty) ->
      let trade =
        {
          Types.trade_id = Printf.sprintf "fill-%f" (Unix.gettimeofday ());
          order_id = Printf.sprintf "order-%f" (Unix.gettimeofday ());
          instrument = signal.instrument;
          side = if signal.action = Types.EnterShort then Types.Sell else Types.Buy;
          quantity = qty;
          price = 100.;
          fee = 0.1;
          timestamp = Types.now ();
        }
      in
      Trading_portfolio.Portfolio_state.apply_trade t.portfolio trade;
      Lwt.return_unit
  | Ok _ -> Lwt.return_unit
  | Error (Types.Risk_error rv) -> Message_bus.publish t.control_bus (Events.RiskAlert rv)
  | Error _ -> Lwt.return_unit

let handle_market_event t event =
  let signals = Strategy_manager.on_market_event t.strategy_mgr event in
  let signals =
    Allocation.resolve t.policy ~weights:(Strategy_manager.strategy_weights t.strategy_mgr) signals
  in
  Lwt_list.iter_p (process_signal t) signals

let start_market_stream t instrument =
  let module V =
    (val Trading_venues.Venue_registry.by_venue Types.Paper : Trading_venues.Venue_intf.VENUE)
  in
  let stream = V.stream_market_data instrument in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else
      let%lwt next = Lwt_stream.get stream in
      match next with
      | None -> Lwt.return_unit
      | Some evt ->
          let%lwt () = handle_market_event t evt in
          loop ()
  in
  loop ()

let start_control_loop t =
  let stream = Message_bus.subscribe t.control_bus in
  let rec loop () =
    if not t.running then Lwt.return_unit
    else
      let%lwt msg = Lwt_stream.get stream in
      match msg with
      | None -> Lwt.return_unit
      | Some (Events.Control cmd) ->
          (match cmd with
          | Events.Enable_strategy id -> enable_strategy t id
          | Events.Disable_strategy id -> disable_strategy t id
          | Events.Replace_strategy cfg ->
              ignore (Strategy_manager.hot_swap t.strategy_mgr cfg : (unit, Types.error) result)
          | Events.Get_status ->
              let s = status t in
              Observability.trace_event "status"
                [ ("running", string_of_bool s.running); ("enabled_count", string_of_int (List.length s.enabled_strategies)) ]
          | Events.Get_portfolio ->
              let p = portfolio_snapshot t in
              Observability.trace_event "portfolio" [ ("equity", string_of_float p.portfolio.equity) ]
          | Events.Trigger_kill_switch -> Trading_risk.Manager.trip_circuit_breaker t.risk_mgr);
          loop ()
      | Some _ -> loop ()
  in
  loop ()

let run t =
  t.running <- true;
  let instruments =
    t.cfg.strategies
    |> List.concat_map (fun s -> s.Config.instruments)
    |> List.sort_uniq (fun a b -> String.compare (Types.instrument_key a) (Types.instrument_key b))
  in
  let tasks = List.map (start_market_stream t) instruments @ [ start_control_loop t ] in
  Lwt.join tasks

let stop t =
  t.running <- false
