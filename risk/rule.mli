open Trading_core.Types

type context = {
  portfolio : Trading_core.Types.portfolio;
  proposed_order : Trading_core.Types.order;
  daily_drawdown : float;
  intraday_pnl : float;
  instrument_volatility : float option;
}

type t = context -> (unit, risk_violation) result

val max_position_size : max_fraction:float -> t
val max_capital_per_asset_class : max_fraction:float -> t
val max_daily_drawdown : threshold:float -> t
val max_intraday_loss : threshold:float -> t
val volatility_adjusted_sizing : target_vol:float -> t
val net_exposure_limit : max_notional:float -> t
