type metric = Counter of float | Gauge of float

type t

val create : unit -> t
val init_logging : unit -> unit
val incr_counter : t -> string -> float -> unit
val set_gauge : t -> string -> float -> unit
val render_prometheus : t -> string
val trace_event : string -> (string * string) list -> unit
val audit_order : string -> unit
