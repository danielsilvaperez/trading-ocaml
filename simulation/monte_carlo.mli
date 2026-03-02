type config = {
  paths : int;
  horizon : int;
  ruin_threshold : float;
  slippage_bps_range : float * float;
  perturbation_std : float;
}

type distributions = {
  returns : float list;
  drawdowns : float list;
  probability_of_ruin : float;
  expected_value : float;
}

val bootstrap_returns : source:float list -> horizon:int -> float list
val run : config:config -> source_returns:float list -> distributions
