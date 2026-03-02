module StringMap = Map.Make (String)

type metric = Counter of float | Gauge of float

type t = {
  mutable metrics : metric StringMap.t;
}

let create () = { metrics = StringMap.empty }

let init_logging () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info)

let incr_counter t name delta =
  let update = function
    | Some (Counter v) -> Some (Counter (v +. delta))
    | Some (Gauge v) -> Some (Gauge (v +. delta))
    | None -> Some (Counter delta)
  in
  t.metrics <- StringMap.update name update t.metrics

let set_gauge t name value =
  t.metrics <- StringMap.add name (Gauge value) t.metrics

let render_prometheus t =
  StringMap.bindings t.metrics
  |> List.map (fun (name, v) ->
         let value = match v with Counter x | Gauge x -> x in
         Printf.sprintf "%s %.10f" name value)
  |> String.concat "\n"

let trace_event name fields =
  let payload =
    fields
    |> List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
    |> String.concat " "
  in
  Logs.info (fun m -> m "trace event=%s %s" name payload)

let audit_order line = Logs.app (fun m -> m "AUDIT %s" line)
