open Trading_core

type t

type sizing_input = {
  bankroll : float;
  edge : float;
  win_probability : float;
  payoff_ratio : float;
  volatility : float option;
}

val create : Config.risk_limits -> t
val evaluate_order :
  t ->
  portfolio:Types.portfolio ->
  daily_drawdown:float ->
  intraday_pnl:float ->
  instrument_volatility:float option ->
  Types.order ->
  (unit, Types.error) result

val recommended_notional : t -> sizing_input -> float
val trip_circuit_breaker : t -> unit
val reset_circuit_breaker : t -> unit
val circuit_breaker_tripped : t -> bool
