open Alcotest

let test_instrument_key () =
  let i = Trading_core.Types.{ symbol = "AAPL"; asset_class = Equity; venue_hint = None } in
  check string "instrument key" "eq:AAPL" (Trading_core.Types.instrument_key i)

let test_accounting_apply_trade () =
  let open Trading_core.Types in
  let portfolio = Trading_portfolio.Accounting.empty ~capital:10_000. in
  let instrument = { symbol = "BTC-USD"; asset_class = Crypto; venue_hint = None } in
  let trade =
    {
      trade_id = "t1";
      order_id = "o1";
      instrument;
      side = Buy;
      quantity = 1.;
      price = 100.;
      fee = 1.;
      timestamp = now ();
    }
  in
  let portfolio = Trading_portfolio.Accounting.apply_trade portfolio trade in
  check (float 0.001) "cash debited" 9_899. portfolio.cash;
  check int "position count" 1 (List.length portfolio.positions)

let () =
  run "domain"
    [
      ("types", [ test_case "instrument_key" `Quick test_instrument_key ]);
      ("portfolio", [ test_case "apply_trade" `Quick test_accounting_apply_trade ]);
    ]
