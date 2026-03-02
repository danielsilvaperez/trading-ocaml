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

let venue_to_string = function
  | Robinhood -> "robinhood"
  | Coinbase -> "coinbase"
  | Alpaca -> "alpaca"
  | Binance -> "binance"
  | Paper -> "paper"
  | Backtest -> "backtest"
  | Simulation -> "simulation"

let instrument_key i =
  let asset = match i.asset_class with Equity -> "eq" | Crypto -> "cr" in
  Printf.sprintf "%s:%s" asset i.symbol

let pp_asset_class fmt = function
  | Equity -> Format.fprintf fmt "Equity"
  | Crypto -> Format.fprintf fmt "Crypto"

let pp_venue fmt v = Format.fprintf fmt "%s" (venue_to_string v)

let pp_risk_violation fmt = function
  | MaxPositionSize i -> Format.fprintf fmt "MaxPositionSize(%s)" (instrument_key i)
  | MaxCapitalByAssetClass a -> Format.fprintf fmt "MaxCapitalByAssetClass(%a)" pp_asset_class a
  | DailyDrawdownLimit d -> Format.fprintf fmt "DailyDrawdownLimit(%.4f)" d
  | IntradayLossLimit d -> Format.fprintf fmt "IntradayLossLimit(%.4f)" d
  | VolatilityAdjustedSizingCap -> Format.fprintf fmt "VolatilityAdjustedSizingCap"
  | KellyCap -> Format.fprintf fmt "KellyCap"
  | CircuitBreaker -> Format.fprintf fmt "CircuitBreaker"
  | NetExposureLimit i -> Format.fprintf fmt "NetExposureLimit(%s)" (instrument_key i)
  | RuleFailure msg -> Format.fprintf fmt "RuleFailure(%s)" msg

let now () = Ptime_clock.now ()
