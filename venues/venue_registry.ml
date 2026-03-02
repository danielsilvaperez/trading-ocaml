open Trading_core.Types

let by_venue = function
  | Robinhood -> (module Robinhood_venue : Venue_intf.VENUE)
  | Coinbase -> (module Coinbase_venue : Venue_intf.VENUE)
  | Alpaca -> (module Alpaca_venue : Venue_intf.VENUE)
  | Binance -> (module Binance_venue : Venue_intf.VENUE)
  | Paper | Backtest | Simulation -> (module Paper_venue : Venue_intf.VENUE)
