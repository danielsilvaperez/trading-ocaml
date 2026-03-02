type 'a t = {
  mutable consumers : ('a option -> unit) list;
  lock : Lwt_mutex.t;
}

let create () = { consumers = []; lock = Lwt_mutex.create () }

let publish t event =
  Lwt_mutex.with_lock t.lock (fun () ->
      List.iter (fun push -> push (Some event)) t.consumers;
      Lwt.return_unit)

let subscribe t =
  let stream, push = Lwt_stream.create () in
  t.consumers <- push :: t.consumers;
  stream
