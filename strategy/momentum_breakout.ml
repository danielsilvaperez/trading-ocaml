open Trading_core
open Common

module M : Strategy_intf.STRATEGY = struct
  type state = {
    strategy_id : string;
    lookback : int;
    history : float list;
  }

  let name = "momentum_breakout"

  let init cfg =
    {
      strategy_id = cfg.Config.id;
      lookback = int_of_float (param cfg "lookback" 40.);
      history = [];
    }

let on_market_event state evt =
    match (extract_close evt, instrument_of_event evt) with
    | Some px, Some instrument ->
        let history = state.history @ [ px ] in
        let window =
          let len = List.length history in
          if len <= state.lookback then history
          else
            let rec drop n xs =
              if n <= 0 then xs
              else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest
            in
            drop (len - state.lookback) history
        in
        let high = List.fold_left max neg_infinity window in
        let low = List.fold_left min infinity window in
        let maybe_signal =
          if px >= high then
            Some
              (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterLong
                 ~conviction:0.7 ~target_notional:None ~reason:"breakout_up")
          else if px <= low then
            Some
              (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterShort
                 ~conviction:0.7 ~target_notional:None ~reason:"breakout_down")
          else None
        in
        ({ state with history }, maybe_signal)
    | _ -> (state, None)
end
