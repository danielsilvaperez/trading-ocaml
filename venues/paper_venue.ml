open Trading_core.Types

type order_book = {
  orders : (order_id, order) Hashtbl.t;
  positions : (string, position) Hashtbl.t;
  prices : (string, float) Hashtbl.t;
  mutable sequence : int;
}

let state =
  {
    orders = Hashtbl.create 256;
    positions = Hashtbl.create 256;
    prices = Hashtbl.create 256;
    sequence = 0;
  }

let name = "paper"

let seed_price instrument px = Hashtbl.replace state.prices (instrument_key instrument) px

let next_id () =
  state.sequence <- state.sequence + 1;
  Printf.sprintf "paper-%08d" state.sequence

let fill_price (intent : order_intent) : float =
  match intent.order_type with
  | Market -> (
      match Hashtbl.find_opt state.prices (instrument_key intent.instrument) with
      | Some p -> p
      | None -> 100.)
  | Limit p | Stop p -> p
  | StopLimit { limit; _ } -> limit

let update_position (intent : order_intent) (px : float) : unit =
  let key = instrument_key intent.instrument in
  let sign = match intent.side with Buy -> 1. | Sell -> -.1. in
  let qty_delta = sign *. intent.quantity in
  let current : position =
    match Hashtbl.find_opt state.positions key with
    | Some p -> p
    | None ->
        {
          instrument = intent.instrument;
          quantity = 0.;
          avg_price = 0.;
          market_price = px;
          realized_pnl = 0.;
          unrealized_pnl = 0.;
        }
  in
  let new_qty = current.quantity +. qty_delta in
  let avg_price =
    if Float.abs new_qty < 1e-9 then 0.
    else if Float.abs current.quantity < 1e-9 then px
    else ((current.avg_price *. current.quantity) +. (px *. qty_delta)) /. new_qty
  in
  let unrealized = (px -. avg_price) *. new_qty in
  let updated =
    {
      current with
      quantity = new_qty;
      avg_price;
      market_price = px;
      unrealized_pnl = unrealized;
    }
  in
  Hashtbl.replace state.positions key updated

let place_order (order : order) =
  let oid = next_id () in
  let order' : order = { order with order_id = oid } in
  Hashtbl.replace state.orders oid order';
  let px = fill_price order'.intent in
  seed_price order'.intent.instrument px;
  update_position order'.intent px;
  Lwt.return (Ok (Filled order'.intent.quantity))

let cancel_order oid =
  Hashtbl.remove state.orders oid;
  Lwt.return_unit

let get_positions () =
  Hashtbl.to_seq_values state.positions |> List.of_seq |> Lwt.return

let stream_market_data instrument =
  let stream, push = Lwt_stream.create () in
  let rec loop last_px =
    let jitter = (Random.float 2.) -. 1. in
    let px = max 0.01 (last_px +. jitter *. 0.2) in
    seed_price instrument px;
    let ts = now () in
    push
      (Some
         (TradeTick { instrument; price = px; size = 1. +. Random.float 10.; timestamp = ts }));
    let%lwt () = Lwt_unix.sleep 0.25 in
    loop px
  in
  let start_px =
    match Hashtbl.find_opt state.prices (instrument_key instrument) with Some p -> p | None -> 100.
  in
  Lwt.async (fun () -> loop start_px);
  stream
