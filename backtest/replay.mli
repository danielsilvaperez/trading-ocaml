open Trading_core

val run : events:Types.market_event list -> on_event:(Types.market_event -> unit) -> unit
