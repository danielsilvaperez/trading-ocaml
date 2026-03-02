open Trading_core

type t =
  | Pack : {
      module_ : (module Strategy_intf.STRATEGY with type state = 's);
      cfg : Config.strategy_instance;
      state : 's;
    }
      -> t

let create (type s) (module S : Strategy_intf.STRATEGY with type state = s) cfg =
  let state = S.init cfg in
  Pack { module_ = (module S); cfg; state }

let id (Pack p) = p.cfg.id

let name (Pack { module_ = (module S); _ }) = S.name

let enabled (Pack p) = p.cfg.enabled

let set_enabled (Pack p) enabled = Pack { p with cfg = { p.cfg with enabled } }
let weight (Pack p) = p.cfg.weight
let instruments (Pack p) = p.cfg.instruments

let on_market_event (Pack ({ module_ = (module S); _ } as p)) event =
  let state', signal = S.on_market_event p.state event in
  (Pack { p with state = state' }, signal)
