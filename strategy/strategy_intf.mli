open Trading_core

module type STRATEGY = sig
  type state

  val name : string
  val init : Config.strategy_instance -> state
  val on_market_event : state -> Types.market_event -> state * Types.signal option
end
