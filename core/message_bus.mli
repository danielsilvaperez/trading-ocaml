type 'a t

val create : unit -> 'a t
val publish : 'a t -> 'a -> unit Lwt.t
val subscribe : 'a t -> 'a Lwt_stream.t
