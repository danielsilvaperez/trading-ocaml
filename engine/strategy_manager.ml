open Trading_core

module SMap = Map.Make (String)

type t = {
  mutable strategies : Trading_strategy.Strategy_pack.t SMap.t;
}

let instrument_scoped_cfg (cfg : Config.strategy_instance) (instrument : Types.instrument) :
    Config.strategy_instance =
  {
    cfg with
    Config.id = cfg.id ^ "::" ^ instrument.Types.symbol;
    instruments = [ instrument ];
  }

let create (configs : Config.strategy_instance list) =
  let add_one acc (cfg : Config.strategy_instance) =
    let scoped =
      match cfg.Config.instruments with
      | [] -> [ cfg ]
      | instruments -> List.map (instrument_scoped_cfg cfg) instruments
    in
    List.fold_left
      (fun acc cfg ->
        match (acc, Trading_strategy.Registry.make cfg) with
        | Error e, _ -> Error e
        | Ok _, Error e -> Error e
        | Ok map, Ok pack -> Ok (SMap.add (Trading_strategy.Strategy_pack.id pack) pack map))
      acc scoped
  in
  let init = Ok SMap.empty in
  match List.fold_left add_one init configs with
  | Error e -> Error e
  | Ok strategies -> Ok { strategies }

let enabled_ids t =
  t.strategies
  |> SMap.bindings
  |> List.filter_map (fun (id, p) -> if Trading_strategy.Strategy_pack.enabled p then Some id else None)

let set_enabled t ~id enabled =
  match SMap.find_opt id t.strategies with
  | None -> ()
  | Some pack ->
      t.strategies <-
        SMap.add id (Trading_strategy.Strategy_pack.set_enabled pack enabled) t.strategies

let hot_swap t (cfg : Config.strategy_instance) =
  match Trading_strategy.Registry.make cfg with
  | Error e -> Error e
  | Ok pack ->
      t.strategies <- SMap.add (Trading_strategy.Strategy_pack.id pack) pack t.strategies;
      Ok ()

let event_matches_instrument event instrument =
  match event with
  | Types.Quote { instrument = i; _ }
  | Types.TradeTick { instrument = i; _ }
  | Types.Bar { instrument = i; _ } -> Types.instrument_key i = Types.instrument_key instrument
  | Types.Heartbeat _ -> true

let on_market_event t event =
  let run_pack (id, pack) =
    if not (Trading_strategy.Strategy_pack.enabled pack) then (id, pack, None)
    else
      let subscribed =
        match Trading_strategy.Strategy_pack.instruments pack with
        | [] -> true
        | ins -> List.exists (event_matches_instrument event) ins
      in
      if not subscribed then (id, pack, None)
      else
        let pack', signal = Trading_strategy.Strategy_pack.on_market_event pack event in
        (id, pack', signal)
  in
  let results = t.strategies |> SMap.bindings |> List.map run_pack in
  t.strategies <- List.fold_left (fun acc (id, pack, _) -> SMap.add id pack acc) SMap.empty results;
  List.filter_map (fun (_, _, s) -> s) results

let strategy_weights t =
  t.strategies
  |> SMap.bindings
  |> List.map (fun (_id, p) ->
         (Trading_strategy.Strategy_pack.id p, Trading_strategy.Strategy_pack.weight p))
