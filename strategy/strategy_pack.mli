open Trading_core

type t

val create :
  (module Strategy_intf.STRATEGY with type state = 's) ->
  Config.strategy_instance ->
  t

val id : t -> string
val name : t -> string
val enabled : t -> bool
val set_enabled : t -> bool -> t
val weight : t -> float
val instruments : t -> Types.instrument list
val on_market_event : t -> Types.market_event -> t * Types.signal option
