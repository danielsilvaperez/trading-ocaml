open Trading_core

type t = {
  limits : Config.risk_limits;
  mutable breaker_tripped : bool;
  rules : Rule.t list;
}

type sizing_input = {
  bankroll : float;
  edge : float;
  win_probability : float;
  payoff_ratio : float;
  volatility : float option;
}

let create limits =
  {
    limits;
    breaker_tripped = false;
    rules =
      [
        Rule.max_position_size ~max_fraction:limits.max_position_size;
        Rule.max_capital_per_asset_class ~max_fraction:limits.max_capital_per_asset_class;
        Rule.max_daily_drawdown ~threshold:limits.max_daily_drawdown;
        Rule.max_intraday_loss ~threshold:limits.max_intraday_loss;
        Rule.volatility_adjusted_sizing ~target_vol:limits.volatility_target;
        Rule.net_exposure_limit ~max_notional:(100_000. *. limits.max_position_size);
      ];
  }

let evaluate_order t ~portfolio ~daily_drawdown ~intraday_pnl ~instrument_volatility order =
  if t.breaker_tripped && t.limits.circuit_breaker_enabled then Error (Types.Risk_error Types.CircuitBreaker)
  else
    let ctx = Rule.{ portfolio; proposed_order = order; daily_drawdown; intraday_pnl; instrument_volatility } in
    let rec loop = function
      | [] -> Ok ()
      | rule :: rest -> (
          match rule ctx with
          | Ok () -> loop rest
          | Error v -> Error (Types.Risk_error v))
    in
    loop t.rules

let kelly_fraction i =
  let p = i.win_probability in
  let b = max 0.0001 i.payoff_ratio in
  max 0. (((p *. (b +. 1.)) -. 1.) /. b)

let recommended_notional t i =
  let raw_kelly = kelly_fraction i in
  let capped_kelly = min t.limits.kelly_cap raw_kelly in
  let vol_adj =
    match i.volatility with
    | None -> 1.
    | Some v when v <= 0. -> 1.
    | Some v -> min 1. (t.limits.volatility_target /. v)
  in
  let edge_adj = max 0. (min 1. i.edge) in
  i.bankroll *. capped_kelly *. vol_adj *. edge_adj

let trip_circuit_breaker t = t.breaker_tripped <- true
let reset_circuit_breaker t = t.breaker_tripped <- false
let circuit_breaker_tripped t = t.breaker_tripped
