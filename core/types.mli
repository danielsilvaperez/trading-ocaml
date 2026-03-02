type asset_class = Equity | Crypto

type venue = Robinhood | Coinbase | Alpaca | Binance | Paper | Backtest | Simulation

type instrument = {
  symbol : string;
  asset_class : asset_class;
  venue_hint : venue option;
}

type order_id = string
type strategy_id = string

type order_side = Buy | Sell
type order_type =
  | Market
  | Limit of float
  | Stop of float
  | StopLimit of { stop : float; limit : float }

type time_in_force = Gtc | Ioc | Fok | Day

type order_status =
  | New
  | Accepted
  | PartiallyFilled of float
  | Filled of float
  | Cancelled
  | Rejected of string

type order_intent = {
  strategy_id : strategy_id;
  instrument : instrument;
  side : order_side;
  quantity : float;
  order_type : order_type;
  tif : time_in_force;
  submitted_at : Ptime.t;
}

type order = {
  order_id : order_id;
  venue : venue;
  intent : order_intent;
}

type trade = {
  trade_id : string;
  order_id : order_id;
  instrument : instrument;
  side : order_side;
  quantity : float;
  price : float;
  fee : float;
  timestamp : Ptime.t;
}

type position = {
  instrument : instrument;
  quantity : float;
  avg_price : float;
  market_price : float;
  realized_pnl : float;
  unrealized_pnl : float;
}

type portfolio = {
  cash : float;
  equity : float;
  positions : position list;
}

type market_event =
  | Quote of {
      instrument : instrument;
      bid : float;
      ask : float;
      timestamp : Ptime.t;
    }
  | TradeTick of {
      instrument : instrument;
      price : float;
      size : float;
      timestamp : Ptime.t;
    }
  | Bar of {
      instrument : instrument;
      open_ : float;
      high : float;
      low : float;
      close : float;
      volume : float;
      timestamp : Ptime.t;
    }
  | Heartbeat of Ptime.t

type signal_action = EnterLong | EnterShort | Exit | Reduce | Hold

type signal = {
  strategy_id : strategy_id;
  instrument : instrument;
  action : signal_action;
  conviction : float;
  target_notional : float option;
  reason : string;
  timestamp : Ptime.t;
}

type risk_violation =
  | MaxPositionSize of instrument
  | MaxCapitalByAssetClass of asset_class
  | DailyDrawdownLimit of float
  | IntradayLossLimit of float
  | VolatilityAdjustedSizingCap
  | KellyCap
  | CircuitBreaker
  | NetExposureLimit of instrument
  | RuleFailure of string

type error =
  | Invalid_input of string
  | Venue_error of string
  | Risk_error of risk_violation
  | Strategy_error of string
  | Config_error of string
  | Internal_error of string

val venue_to_string : venue -> string
val instrument_key : instrument -> string
val pp_asset_class : Format.formatter -> asset_class -> unit
val pp_venue : Format.formatter -> venue -> unit
val pp_risk_violation : Format.formatter -> risk_violation -> unit
val now : unit -> Ptime.t
