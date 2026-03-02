open Trading_core

type execution_model = {
  base_slippage_bps : float;
  commission_bps : float;
  latency_ms : int;
}

type summary = {
  start_equity : float;
  end_equity : float;
  total_return : float;
  max_drawdown : float;
  sharpe : float;
  sortino : float;
  trades : int;
}

let apply_execution_model (model : execution_model) ~side ~price =
  let slip = model.base_slippage_bps /. 10_000. in
  let signed = if side = Types.Buy then 1. else -.1. in
  let fill = price *. (1. +. (signed *. slip)) in
  let fee = price *. (model.commission_bps /. 10_000.) in
  (fill, fee)

let run ~config ~csv_path ~instrument ~execution_model =
  match Trading_engine.Strategy_manager.create config.Config.strategies with
  | Error e -> Error e
  | Ok strategy_mgr -> (
      match Csv_ingest.load_bars ~instrument ~path:csv_path with
      | Error e -> Error e
      | Ok events ->
          let risk_mgr = Trading_risk.Manager.create config.risk in
          let portfolio = Trading_portfolio.Portfolio_state.create ~capital:config.initial_capital in
          let trades = ref 0 in
          let run_signal signal =
            let snap = Trading_portfolio.Portfolio_state.snapshot portfolio in
            let side = if signal.Types.action = Types.EnterShort then Types.Sell else Types.Buy in
            let price =
              match signal.target_notional with
              | Some notional when signal.conviction > 0. -> notional /. (10. *. signal.conviction)
              | _ -> 100.
            in
            let qty = max 0.001 (signal.conviction *. 10.) in
            let order =
              {
                Types.order_id = "bt-order";
                venue = Types.Backtest;
                intent =
                  {
                    strategy_id = signal.strategy_id;
                    instrument = signal.instrument;
                    side;
                    quantity = qty;
                    order_type = Types.Market;
                    tif = Types.Day;
                    submitted_at = Types.now ();
                  };
              }
            in
            match
              Trading_risk.Manager.evaluate_order risk_mgr ~portfolio:snap.portfolio
                ~daily_drawdown:snap.drawdown
                ~intraday_pnl:(snap.realized_pnl +. snap.unrealized_pnl) ~instrument_volatility:None order
            with
            | Error _ -> ()
            | Ok () ->
                let fill, fee = apply_execution_model execution_model ~side ~price in
                let trade =
                  {
                    Types.trade_id = Printf.sprintf "bt-trade-%d" !trades;
                    order_id = order.order_id;
                    instrument = order.intent.instrument;
                    side = order.intent.side;
                    quantity = order.intent.quantity;
                    price = fill;
                    fee;
                    timestamp = Types.now ();
                  }
                in
                incr trades;
                Trading_portfolio.Portfolio_state.apply_trade portfolio trade
          in
          Replay.run ~events ~on_event:(fun evt ->
              let signals = Trading_engine.Strategy_manager.on_market_event strategy_mgr evt in
              let signals =
                Trading_engine.Allocation.resolve Trading_engine.Allocation.Independent
                  ~weights:(Trading_engine.Strategy_manager.strategy_weights strategy_mgr)
                  signals
              in
              List.iter run_signal signals;
              match evt with
              | Types.Bar { instrument; close; _ } ->
                  Trading_portfolio.Portfolio_state.mark_to_market portfolio instrument ~price:close
              | Types.TradeTick { instrument; price; _ } ->
                  Trading_portfolio.Portfolio_state.mark_to_market portfolio instrument ~price
              | _ -> ());
          let snap = Trading_portfolio.Portfolio_state.snapshot portfolio in
          let returns =
            let curve = Trading_portfolio.Portfolio_state.equity_curve portfolio in
            let rec r acc = function
              | a :: (b :: _ as rest) when a > 0. -> r (((b /. a) -. 1.) :: acc) rest
              | _ :: rest -> r acc rest
              | [] -> List.rev acc
            in
            r [] curve
          in
          let start_equity = config.initial_capital in
          let end_equity = snap.portfolio.equity in
          Ok
            {
              start_equity;
              end_equity;
              total_return = if start_equity <= 0. then 0. else (end_equity /. start_equity) -. 1.;
              max_drawdown = snap.drawdown;
              sharpe = Trading_portfolio.Metrics.sharpe_ratio ~returns ~risk_free_rate:0.0;
              sortino = Trading_portfolio.Metrics.sortino_ratio ~returns ~risk_free_rate:0.0;
              trades = !trades;
            })
