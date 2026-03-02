type config = {
  paths : int;
  horizon : int;
  ruin_threshold : float;
  slippage_bps_range : float * float;
  perturbation_std : float;
}

type distributions = {
  returns : float list;
  drawdowns : float list;
  probability_of_ruin : float;
  expected_value : float;
}

let gaussian ~std =
  let u1 = max 1e-10 (Random.float 1.) in
  let u2 = Random.float 1. in
  let z0 = sqrt (-2. *. log u1) *. cos (2. *. Float.pi *. u2) in
  z0 *. std

let bootstrap_returns ~source ~horizon =
  if source = [] then []
  else
    let len = List.length source in
    let rec loop acc n =
      if n <= 0 then List.rev acc
      else
        let idx = Random.int len in
        let r = List.nth source idx in
        loop (r :: acc) (n - 1)
    in
    loop [] horizon

let drawdown curve =
  let rec loop peak max_dd = function
    | [] -> max_dd
    | x :: rest ->
        let peak = max peak x in
        let dd = if peak <= 0. then 0. else (peak -. x) /. peak in
        loop peak (max max_dd dd) rest
  in
  loop 1. 0. curve

let apply_path_perturbation cfg rs =
  let slip_lo, slip_hi = cfg.slippage_bps_range in
  List.map
    (fun r ->
      let perturb = gaussian ~std:cfg.perturbation_std in
      let slippage = (slip_lo +. Random.float (max 0. (slip_hi -. slip_lo))) /. 10_000. in
      r +. perturb -. slippage)
    rs

let path_to_curve rs =
  let rec loop acc value = function
    | [] -> List.rev (value :: acc)
    | r :: rest ->
        let next = value *. (1. +. r) in
        loop (value :: acc) next rest
  in
  loop [] 1. rs

let run ~config ~source_returns =
  Random.self_init ();
  let returns = ref [] in
  let drawdowns = ref [] in
  let ruin_count = ref 0 in
  for _ = 1 to config.paths do
    let sampled = bootstrap_returns ~source:source_returns ~horizon:config.horizon in
    let perturbed = apply_path_perturbation config sampled in
    let curve = path_to_curve perturbed in
    let final = List.hd (List.rev curve) in
    let total_return = final -. 1. in
    let max_dd = drawdown curve in
    if final <= config.ruin_threshold then incr ruin_count;
    returns := total_return :: !returns;
    drawdowns := max_dd :: !drawdowns
  done;
  let rs = List.rev !returns in
  {
    returns = rs;
    drawdowns = List.rev !drawdowns;
    probability_of_ruin = float_of_int !ruin_count /. float_of_int (max 1 config.paths);
    expected_value =
      (match rs with
      | [] -> 0.
      | _ -> List.fold_left ( +. ) 0. rs /. float_of_int (List.length rs));
  }
