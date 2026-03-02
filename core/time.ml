let parse_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Ok t
  | Error _ -> Error ("invalid RFC3339 timestamp: " ^ s)

let parse_date s =
  let candidate = s ^ "T00:00:00Z" in
  parse_rfc3339 candidate

let to_rfc3339 t = Ptime.to_rfc3339 t
