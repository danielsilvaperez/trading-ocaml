open Trading_core

type t

val create : Config.t -> t
val run : t -> unit Lwt.t
val stop : t -> unit
val status : t -> Events.status
val enable_strategy : t -> string -> unit
val disable_strategy : t -> string -> unit
val portfolio_snapshot : t -> Trading_portfolio.Portfolio_state.snapshot
val bus : t -> Events.bus_message Trading_core.Message_bus.t
