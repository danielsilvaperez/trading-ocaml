let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let tail_window ~window xs =
  let len = List.length xs in
  if len <= window then xs else drop (len - window) xs

let sma ~window xs =
  let xs = tail_window ~window xs in
  if xs = [] then None
  else
    let sum = List.fold_left ( +. ) 0. xs in
    Some (sum /. float_of_int (List.length xs))

let stddev ~window xs =
  let xs = tail_window ~window xs in
  match sma ~window xs with
  | None -> None
  | Some m ->
      let n = List.length xs in
      if n < 2 then None
      else
        let var =
          List.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0. xs /. float_of_int (n - 1)
        in
        Some (sqrt var)

let vwap ~prices ~volumes =
  let pv = List.fold_left2 (fun acc p v -> acc +. (p *. v)) 0. prices volumes in
  let vol = List.fold_left ( +. ) 0. volumes in
  if vol <= 0. then None else Some (pv /. vol)
