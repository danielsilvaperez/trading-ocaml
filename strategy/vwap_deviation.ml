open Trading_core
open Common

module M : Strategy_intf.STRATEGY = struct
  type state = {
    strategy_id : string;
    lookback : int;
    prices : float list;
    volumes : float list;
    threshold : float;
  }

  let name = "vwap_deviation"

  let init cfg =
    {
      strategy_id = cfg.Config.id;
      lookback = int_of_float (param cfg "lookback" 50.);
      prices = [];
      volumes = [];
      threshold = param cfg "threshold" 0.015;
    }

  let on_market_event state evt =
    match evt with
    | Types.Bar { instrument; close; volume; _ } ->
        let prices = state.prices @ [ close ] in
        let volumes = state.volumes @ [ volume ] in
        let trim xs =
          let len = List.length xs in
          if len <= state.lookback then xs
          else
            let rec drop n ys =
              if n <= 0 then ys
              else match ys with [] -> [] | _ :: rest -> drop (n - 1) rest
            in
            drop (len - state.lookback) xs
        in
        let prices = trim prices in
        let volumes = trim volumes in
        let maybe_signal =
          match Indicators.vwap ~prices ~volumes with
          | Some vwap when vwap > 0. ->
              let dev = (close -. vwap) /. vwap in
              if dev <= -.state.threshold then
                Some
                  (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterLong
                     ~conviction:(Float.min 1. (Float.abs dev /. 0.05)) ~target_notional:None
                     ~reason:"vwap_negative_deviation")
              else if dev >= state.threshold then
                Some
                  (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterShort
                     ~conviction:(Float.min 1. (Float.abs dev /. 0.05)) ~target_notional:None
                     ~reason:"vwap_positive_deviation")
              else None
          | _ -> None
        in
        ({ state with prices; volumes }, maybe_signal)
    | _ -> (state, None)
end
