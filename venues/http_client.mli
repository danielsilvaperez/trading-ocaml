val post_json :
  base_url:string ->
  path:string ->
  headers:(string * string) list ->
  body:string ->
  (string, Trading_core.Types.error) result Lwt.t
