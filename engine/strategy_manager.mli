open Trading_core

type t

val create : Config.strategy_instance list -> (t, Types.error) result
val enabled_ids : t -> string list
val set_enabled : t -> id:string -> bool -> unit
val hot_swap : t -> Config.strategy_instance -> (unit, Types.error) result
val on_market_event : t -> Types.market_event -> Types.signal list
val strategy_weights : t -> (string * float) list
