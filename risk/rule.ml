open Trading_core.Types

type context = {
  portfolio : Trading_core.Types.portfolio;
  proposed_order : Trading_core.Types.order;
  daily_drawdown : float;
  intraday_pnl : float;
  instrument_volatility : float option;
}

type t = context -> (unit, risk_violation) result

let equity ctx = max 0.01 ctx.portfolio.equity

let order_notional ctx =
  let px =
    match ctx.proposed_order.intent.order_type with
    | Market -> 100.
    | Limit p | Stop p -> p
    | StopLimit { limit; _ } -> limit
  in
  px *. ctx.proposed_order.intent.quantity

let max_position_size ~max_fraction ctx =
  let notional = order_notional ctx in
  if notional <= (equity ctx *. max_fraction) then Ok ()
  else Error (MaxPositionSize ctx.proposed_order.intent.instrument)

let max_capital_per_asset_class ~max_fraction ctx =
  let asset = ctx.proposed_order.intent.instrument.asset_class in
  let exposure =
    ctx.portfolio.positions
    |> List.filter (fun (p : position) -> p.instrument.asset_class = asset)
    |> List.fold_left (fun acc (p : position) -> acc +. Float.abs (p.quantity *. p.market_price)) 0.
  in
  if exposure <= (equity ctx *. max_fraction) then Ok () else Error (MaxCapitalByAssetClass asset)

let max_daily_drawdown ~threshold ctx =
  if ctx.daily_drawdown <= threshold then Ok () else Error (DailyDrawdownLimit ctx.daily_drawdown)

let max_intraday_loss ~threshold ctx =
  if ctx.intraday_pnl >= -.threshold then Ok () else Error (IntradayLossLimit ctx.intraday_pnl)

let volatility_adjusted_sizing ~target_vol ctx =
  match ctx.instrument_volatility with
  | None -> Ok ()
  | Some vol when vol <= 0. -> Ok ()
  | Some vol ->
      let notional = order_notional ctx in
      let scaled_cap = equity ctx *. (target_vol /. vol) in
      if notional <= scaled_cap then Ok () else Error VolatilityAdjustedSizingCap

let net_exposure_limit ~max_notional ctx =
  let key = Trading_core.Types.instrument_key ctx.proposed_order.intent.instrument in
  let existing =
    ctx.portfolio.positions
    |> List.find_opt (fun (p : position) -> Trading_core.Types.instrument_key p.instrument = key)
    |> Option.map (fun (p : position) -> Float.abs (p.quantity *. p.market_price))
    |> Option.value ~default:0.
  in
  if existing +. order_notional ctx <= max_notional then Ok ()
  else Error (NetExposureLimit ctx.proposed_order.intent.instrument)
