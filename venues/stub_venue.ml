open Trading_core.Types

module Make (X : sig
  val venue_name : string
end) = struct
  let name = String.lowercase_ascii X.venue_name

  let place_order order =
    let _payload =
      Printf.sprintf
        "{\"symbol\":\"%s\",\"qty\":%.8f}"
        order.intent.instrument.symbol
        order.intent.quantity
    in
    let%lwt _ =
      Http_client.post_json ~base_url:("https://api." ^ name ^ ".example") ~path:"/orders" ~headers:[]
        ~body:"{}"
    in
    Lwt.return (Ok Accepted)

  let cancel_order _oid = Lwt.return_unit
  let get_positions () = Lwt.return []

  let stream_market_data instrument =
    (* Adapters may switch to websocket transport while preserving this API. *)
    Paper_venue.stream_market_data instrument
end
