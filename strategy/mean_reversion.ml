open Trading_core
open Common

module M : Strategy_intf.STRATEGY = struct
  type state = {
    strategy_id : string;
    lookback : int;
    z_entry : float;
    z_exit : float;
    history : float list;
  }

  let name = "mean_reversion"

  let init cfg =
    {
      strategy_id = cfg.Config.id;
      lookback = int_of_float (param cfg "lookback" 30.);
      z_entry = param cfg "z_entry" 2.0;
      z_exit = param cfg "z_exit" 0.5;
      history = [];
    }

  let on_market_event state evt =
    match (extract_close evt, instrument_of_event evt) with
    | Some px, Some instrument ->
        let history = state.history @ [ px ] in
        let mu = Indicators.sma ~window:state.lookback history in
        let sigma = Indicators.stddev ~window:state.lookback history in
        let maybe_signal =
          match (mu, sigma) with
          | Some m, Some s when s > 0. ->
              let z = (px -. m) /. s in
              if z <= -.state.z_entry then
                Some
                  (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.EnterLong
                     ~conviction:(Float.min 1. (Float.abs z /. 3.0)) ~target_notional:None
                     ~reason:"price_far_below_mean")
              else if Float.abs z <= state.z_exit then
                Some
                  (signal ~strategy_id:state.strategy_id ~instrument ~action:Types.Exit
                     ~conviction:0.3 ~target_notional:None ~reason:"mean_reversion_exit")
              else None
          | _ -> None
        in
        ({ state with history }, maybe_signal)
    | _ -> (state, None)
end
