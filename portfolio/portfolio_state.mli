open Trading_core

type t

type snapshot = {
  portfolio : Types.portfolio;
  realized_pnl : float;
  unrealized_pnl : float;
  twr : float;
  drawdown : float;
  sharpe : float;
  sortino : float;
  exposure_by_asset : (Types.asset_class * float) list;
}

val create : capital:float -> t
val apply_trade : t -> Types.trade -> unit
val mark_to_market : t -> Types.instrument -> price:float -> unit
val snapshot : t -> snapshot
val equity_curve : t -> float list
