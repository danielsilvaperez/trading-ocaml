open Trading_core

type t

type routing_env = {
  risk : Trading_risk.Manager.t;
  portfolio : Trading_portfolio.Portfolio_state.t;
  observability : Trading_core.Observability.t;
  default_venue : Types.venue;
}

val create : routing_env -> t

val route_signal :
  t ->
  daily_drawdown:float ->
  intraday_pnl:float ->
  instrument_volatility:float option ->
  Types.signal ->
  (Types.order_status, Types.error) result Lwt.t
