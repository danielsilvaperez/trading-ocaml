val time_weighted_return : equity_curve:float list -> float
val drawdown_series : equity_curve:float list -> float list
val max_drawdown : equity_curve:float list -> float
val sharpe_ratio : returns:float list -> risk_free_rate:float -> float
val sortino_ratio : returns:float list -> risk_free_rate:float -> float
