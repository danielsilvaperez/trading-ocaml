open Trading_core
open Common

module M : Strategy_intf.STRATEGY = struct
  type state = {
    strategy_id : string;
    fast : int;
    slow : int;
    history : float list;
    last_diff : float option;
  }

  let name = "sma"

  let init cfg =
    {
      strategy_id = cfg.Config.id;
      fast = int_of_float (param cfg "fast" 20.);
      slow = int_of_float (param cfg "slow" 50.);
      history = [];
      last_diff = None;
    }

  let on_market_event state evt =
    match (extract_close evt, instrument_of_event evt) with
    | Some px, Some instrument ->
        let history = state.history @ [ px ] in
        let fast = Indicators.sma ~window:state.fast history in
        let slow = Indicators.sma ~window:state.slow history in
        let signal =
          match (fast, slow) with
          | Some f, Some s ->
              let diff = f -. s in
              let next_state = { state with history; last_diff = Some diff } in
              let maybe_signal =
                match state.last_diff with
                | Some d when d <= 0. && diff > 0. ->
                    Some
                      (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterLong
                         ~conviction:(Float.min 1. (Float.abs diff /. s)) ~target_notional:None
                         ~reason:"fast_sma_crossed_above_slow")
                | Some d when d >= 0. && diff < 0. ->
                    Some
                      (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.Exit
                         ~conviction:(Float.min 1. (Float.abs diff /. s)) ~target_notional:None
                         ~reason:"fast_sma_crossed_below_slow")
                | _ -> None
              in
              (next_state, maybe_signal)
          | _ -> ({ state with history }, None)
        in
        signal
    | _ -> (state, None)
end
