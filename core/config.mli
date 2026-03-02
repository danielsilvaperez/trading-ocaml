open Types

type execution_mode = Live | Paper | Backtest | Simulation

type risk_limits = {
  max_position_size : float;
  max_capital_per_asset_class : float;
  max_daily_drawdown : float;
  max_intraday_loss : float;
  kelly_cap : float;
  volatility_target : float;
  circuit_breaker_enabled : bool;
}

type strategy_instance = {
  id : string;
  name : string;
  enabled : bool;
  weight : float;
  instruments : instrument list;
  params : (string * float) list;
}

type venue_credentials = {
  venue : venue;
  api_key : string option;
  api_secret : string option;
  base_url : string option;
}

type interface_config = {
  web_port : int;
  telegram_token : string option;
}

type t = {
  mode : execution_mode;
  initial_capital : float;
  venues : venue_credentials list;
  strategies : strategy_instance list;
  risk : risk_limits;
  interfaces : interface_config;
}

val default : t
val load : string -> (t, Types.error) result
val mode_of_string : string -> (execution_mode, Types.error) result
