open Trading_core.Types

module type VENUE = sig
  val name : string
  val place_order : order -> (order_status, error) result Lwt.t
  val cancel_order : order_id -> unit Lwt.t
  val get_positions : unit -> position list Lwt.t
  val stream_market_data : instrument -> market_event Lwt_stream.t
end
