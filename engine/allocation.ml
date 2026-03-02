open Trading_core

type policy = Independent | EnsembleVoting | RiskParity

let normalize weights =
  let sum = List.fold_left (fun acc (_, w) -> acc +. max 0. w) 0. weights in
  if sum <= 0. then weights else List.map (fun (k, w) -> (k, max 0. w /. sum)) weights

let weight_of weights sid = match List.assoc_opt sid weights with Some w -> w | None -> 1.

let resolve policy ~weights signals =
  match policy with
  | Independent -> signals
  | EnsembleVoting ->
      let by_instrument =
        List.fold_left
          (fun acc s ->
            let k = Types.instrument_key s.Types.instrument in
            let prev = match List.assoc_opt k acc with Some xs -> xs | None -> [] in
            (k, s :: prev) :: List.remove_assoc k acc)
          [] signals
      in
      List.filter_map
        (fun (_k, xs) ->
          let long_score, short_score, exit_score =
            List.fold_left
              (fun (l, sh, e) s ->
                match s.Types.action with
                | Types.EnterLong -> (l +. s.conviction, sh, e)
                | Types.EnterShort -> (l, sh +. s.conviction, e)
                | Types.Exit | Types.Reduce -> (l, sh, e +. s.conviction)
                | Types.Hold -> (l, sh, e))
              (0., 0., 0.) xs
          in
          let base = List.hd xs in
          if long_score > short_score && long_score > exit_score then
            Some { base with action = Types.EnterLong; conviction = min 1. long_score }
          else if short_score > long_score && short_score > exit_score then
            Some { base with action = Types.EnterShort; conviction = min 1. short_score }
          else if exit_score > 0. then Some { base with action = Types.Exit; conviction = min 1. exit_score }
          else None)
        by_instrument
  | RiskParity ->
      let w = normalize weights in
      List.map
        (fun s ->
          let factor = weight_of w s.Types.strategy_id in
          {
            s with
            conviction = min 1. (s.conviction *. factor);
            target_notional = Option.map (fun n -> n *. factor) s.target_notional;
          })
        signals
