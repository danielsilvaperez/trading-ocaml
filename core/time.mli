val parse_rfc3339 : string -> (Ptime.t, string) result
val parse_date : string -> (Ptime.t, string) result
val to_rfc3339 : Ptime.t -> string
