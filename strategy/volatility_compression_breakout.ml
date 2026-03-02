open Trading_core
open Common

module M : Strategy_intf.STRATEGY = struct
  type state = {
    strategy_id : string;
    lookback : int;
    compression_ratio : float;
    history : float list;
  }

  let name = "volatility_compression_breakout"

  let init cfg =
    {
      strategy_id = cfg.Config.id;
      lookback = int_of_float (param cfg "lookback" 40.);
      compression_ratio = param cfg "compression_ratio" 0.5;
      history = [];
    }

  let on_market_event state evt =
    match (extract_close evt, instrument_of_event evt) with
    | Some px, Some instrument ->
        let history = state.history @ [ px ] in
        let short = Indicators.stddev ~window:(max 5 (state.lookback / 4)) history in
        let long = Indicators.stddev ~window:state.lookback history in
        let maybe_signal =
          match (short, long) with
          | Some s, Some l when l > 0. && s /. l <= state.compression_ratio ->
              Some
                (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterLong
                   ~conviction:(Float.min 1. (1. -. (s /. l))) ~target_notional:None
                   ~reason:"vol_compression_breakout")
          | _ -> None
        in
        ({ state with history }, maybe_signal)
    | _ -> (state, None)
end
