open Trading_core

let extract_close = function
  | Types.Bar { close; _ } -> Some close
  | Types.TradeTick { price; _ } -> Some price
  | _ -> None

let instrument_of_event = function
  | Types.Bar { instrument; _ }
  | Types.Quote { instrument; _ }
  | Types.TradeTick { instrument; _ } -> Some instrument
  | Types.Heartbeat _ -> None

let signal ~strategy_id ~instrument ~action ~conviction ~target_notional ~reason =
  ({
     strategy_id;
     instrument;
     action;
     conviction;
     target_notional;
     reason;
     timestamp = Types.now ();
   }
    : Types.signal)

let param cfg key default =
  match List.assoc_opt key cfg.Config.params with Some v -> v | None -> default
