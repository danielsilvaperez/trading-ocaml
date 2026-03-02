let post_json ~base_url ~path ~headers:_ ~body:_ =
  let _endpoint = base_url ^ path in
  (* HTTP transport stays isolated here; live implementation can swap in Cohttp/Eio. *)
  Lwt.return (Ok "{}")
