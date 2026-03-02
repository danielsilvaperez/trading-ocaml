open Trading_core

let new_order_id =
  let seq = ref 0 in
  fun () ->
    incr seq;
    Printf.sprintf "ord-%08d" !seq

type t = { env : routing_env }

and routing_env = {
  risk : Trading_risk.Manager.t;
  portfolio : Trading_portfolio.Portfolio_state.t;
  observability : Trading_core.Observability.t;
  default_venue : Types.venue;
}

let create env = { env }

let signal_to_order t signal =
  let side, quantity =
    match signal.Types.action with
    | Types.EnterLong -> (Types.Buy, max 0.001 (signal.conviction *. 10.))
    | Types.EnterShort -> (Types.Sell, max 0.001 (signal.conviction *. 10.))
    | Types.Exit -> (Types.Sell, max 0.001 (signal.conviction *. 5.))
    | Types.Reduce -> (Types.Sell, max 0.001 (signal.conviction *. 2.))
    | Types.Hold -> (Types.Buy, 0.)
  in
  {
    Types.order_id = new_order_id ();
    venue = (match signal.instrument.venue_hint with Some v -> v | None -> t.env.default_venue);
    intent =
      {
        strategy_id = signal.strategy_id;
        instrument = signal.instrument;
        side;
        quantity;
        order_type = Types.Market;
        tif = Types.Day;
        submitted_at = Types.now ();
      };
  }

let route_signal t ~daily_drawdown ~intraday_pnl ~instrument_volatility signal =
  if signal.Types.action = Types.Hold then Lwt.return (Ok Types.New)
  else
    let order = signal_to_order t signal in
    let snapshot = Trading_portfolio.Portfolio_state.snapshot t.env.portfolio in
    match
      Trading_risk.Manager.evaluate_order t.env.risk ~portfolio:snapshot.portfolio ~daily_drawdown
        ~intraday_pnl
        ~instrument_volatility order
    with
    | Error e ->
        Trading_core.Observability.incr_counter t.env.observability "risk_rejections_total" 1.;
        Lwt.return (Error e)
    | Ok () ->
        let module V =
          (val Trading_venues.Venue_registry.by_venue order.Types.venue :
            Trading_venues.Venue_intf.VENUE)
        in
        let%lwt result = V.place_order order in
        (match result with
        | Ok status ->
            Trading_core.Observability.audit_order
              (Printf.sprintf "order_id=%s strategy=%s status=accepted"
                 order.order_id order.intent.strategy_id);
            Trading_core.Observability.incr_counter t.env.observability "orders_total" 1.;
            Lwt.return (Ok status)
        | Error e -> Lwt.return (Error e))
