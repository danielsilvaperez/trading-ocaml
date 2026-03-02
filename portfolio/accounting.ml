open Trading_core.Types

let empty ~capital = { cash = capital; equity = capital; positions = [] }

let update_position (positions : position list) (trade : trade) : position list =
  let key = instrument_key trade.instrument in
  let side_sign = match trade.side with Buy -> 1. | Sell -> -.1. in
  let qty_delta = side_sign *. trade.quantity in
  let rec walk (acc : position list) = function
    | [] ->
        let avg = trade.price in
        List.rev
          ({
             instrument = trade.instrument;
             quantity = qty_delta;
             avg_price = avg;
             market_price = trade.price;
             realized_pnl = -.trade.fee;
             unrealized_pnl = 0.;
           }
          :: acc)
    | (p : position) :: rest when instrument_key p.instrument = key ->
        let new_qty = p.quantity +. qty_delta in
        let avg_price =
          if Float.abs new_qty < 1e-9 then 0.
          else ((p.avg_price *. p.quantity) +. (trade.price *. qty_delta)) /. new_qty
        in
        let realized_delta =
          if p.quantity *. qty_delta < 0. then
            let closed = min (Float.abs qty_delta) (Float.abs p.quantity) in
            let pnl_per_unit =
              if p.quantity > 0. then trade.price -. p.avg_price else p.avg_price -. trade.price
            in
            (closed *. pnl_per_unit) -. trade.fee
          else -.trade.fee
        in
        let updated =
          {
            p with
            quantity = new_qty;
            avg_price;
            market_price = trade.price;
            realized_pnl = p.realized_pnl +. realized_delta;
            unrealized_pnl = (trade.price -. avg_price) *. new_qty;
          }
        in
        List.rev_append acc (updated :: rest)
    | (p : position) :: rest -> walk (p :: acc) rest
  in
  walk [] positions

let recompute_equity (cash : float) (positions : position list) =
  let gross =
    List.fold_left (fun acc (p : position) -> acc +. (p.quantity *. p.market_price)) 0. positions
  in
  cash +. gross

let apply_trade (portfolio : portfolio) (trade : trade) : portfolio =
  let gross = trade.quantity *. trade.price in
  let cash_delta = if trade.side = Buy then -.gross -. trade.fee else gross -. trade.fee in
  let cash = portfolio.cash +. cash_delta in
  let positions = update_position portfolio.positions trade in
  { cash; positions; equity = recompute_equity cash positions }

let mark_to_market (portfolio : portfolio) (instrument : instrument) ~price : portfolio =
  let positions =
    List.map
      (fun (p : position) ->
        if instrument_key p.instrument = instrument_key instrument then
          { p with market_price = price; unrealized_pnl = (price -. p.avg_price) *. p.quantity }
        else p)
      portfolio.positions
  in
  { portfolio with positions; equity = recompute_equity portfolio.cash positions }

let exposure_by_asset_class (portfolio : portfolio) =
  let add map asset value =
    let prev = match List.assoc_opt asset map with Some x -> x | None -> 0. in
    (asset, prev +. value) :: List.remove_assoc asset map
  in
  List.fold_left
    (fun acc (p : position) ->
      add acc p.instrument.asset_class (Float.abs (p.quantity *. p.market_price)))
    [] portfolio.positions
