open QCheck

let order_with_notional notional =
  let open Trading_core.Types in
  {
    order_id = "o";
    venue = Paper;
    intent =
      {
        strategy_id = "s";
        instrument = { symbol = "AAPL"; asset_class = Equity; venue_hint = None };
        side = Buy;
        quantity = notional /. 100.;
        order_type = Limit 100.;
        tif = Day;
        submitted_at = now ();
      };
  }

let portfolio equity = Trading_portfolio.Accounting.empty ~capital:equity

let prop_max_position_size =
  Test.make ~count:200 ~name:"max_position_size rejects too-large order"
    (pair (float_range 10_000. 1_000_000.) (float_range 0.01 0.5))
    (fun (equity, max_frac) ->
      let limit = equity *. max_frac in
      let order = order_with_notional (limit *. 1.25) in
      let ctx =
        Trading_risk.Rule.
          {
            portfolio = portfolio equity;
            proposed_order = order;
            daily_drawdown = 0.;
            intraday_pnl = 0.;
            instrument_volatility = None;
          }
      in
      match Trading_risk.Rule.max_position_size ~max_fraction:max_frac ctx with
      | Ok () -> false
      | Error _ -> true)

let () =
  let exit_code = QCheck_base_runner.run_tests [ prop_max_position_size ] in
  if exit_code <> 0 then exit exit_code
