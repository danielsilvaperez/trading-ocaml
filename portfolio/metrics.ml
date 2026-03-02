let returns_from_curve curve =
  let rec loop acc = function
    | a :: (b :: _ as rest) when a > 0. -> loop (((b /. a) -. 1.) :: acc) rest
    | _ :: rest -> loop acc rest
    | [] -> List.rev acc
  in
  loop [] curve

let time_weighted_return ~equity_curve =
  returns_from_curve equity_curve |> List.fold_left (fun acc r -> acc *. (1. +. r)) 1. |> fun x -> x -. 1.

let drawdown_series ~equity_curve =
  let rec loop peak acc = function
    | [] -> List.rev acc
    | x :: rest ->
        let peak = max peak x in
        let dd = if peak <= 0. then 0. else (peak -. x) /. peak in
        loop peak (dd :: acc) rest
  in
  loop 0. [] equity_curve

let max_drawdown ~equity_curve =
  drawdown_series ~equity_curve |> List.fold_left max 0.

let mean xs =
  match xs with [] -> 0. | _ -> List.fold_left ( +. ) 0. xs /. float_of_int (List.length xs)

let variance xs =
  match xs with
  | [] | [ _ ] -> 0.
  | _ ->
      let m = mean xs in
      List.fold_left (fun acc x -> acc +. ((x -. m) *. (x -. m))) 0. xs
      /. float_of_int (List.length xs - 1)

let sharpe_ratio ~returns ~risk_free_rate =
  if returns = [] then 0.
  else
    let excess = List.map (fun r -> r -. (risk_free_rate /. 252.)) returns in
    let sigma = sqrt (variance excess) in
    if sigma <= 0. then 0. else (mean excess /. sigma) *. sqrt 252.

let sortino_ratio ~returns ~risk_free_rate =
  if returns = [] then 0.
  else
    let target = risk_free_rate /. 252. in
    let downside = List.filter (fun r -> r < target) returns in
    let sigma_d = sqrt (variance downside) in
    if sigma_d <= 0. then 0. else ((mean returns -. target) /. sigma_d) *. sqrt 252.
