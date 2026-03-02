open Types

type execution_mode = Live | Paper | Backtest | Simulation

type risk_limits = {
  max_position_size : float;
  max_capital_per_asset_class : float;
  max_daily_drawdown : float;
  max_intraday_loss : float;
  kelly_cap : float;
  volatility_target : float;
  circuit_breaker_enabled : bool;
}

type strategy_instance = {
  id : string;
  name : string;
  enabled : bool;
  weight : float;
  instruments : instrument list;
  params : (string * float) list;
}

type venue_credentials = {
  venue : venue;
  api_key : string option;
  api_secret : string option;
  base_url : string option;
}

type interface_config = {
  web_port : int;
  telegram_token : string option;
}

type t = {
  mode : execution_mode;
  initial_capital : float;
  venues : venue_credentials list;
  strategies : strategy_instance list;
  risk : risk_limits;
  interfaces : interface_config;
}

let mode_of_string = function
  | "live" -> Ok Live
  | "paper" -> Ok Paper
  | "backtest" -> Ok Backtest
  | "simulation" -> Ok Simulation
  | raw -> Error (Config_error ("unsupported mode: " ^ raw))

let venue_of_string = function
  | "robinhood" -> Ok Robinhood
  | "coinbase" -> Ok Coinbase
  | "alpaca" -> Ok Alpaca
  | "binance" -> Ok Binance
  | "paper" -> Ok Paper
  | raw -> Error (Config_error ("unsupported venue: " ^ raw))

let default_risk =
  {
    max_position_size = 0.10;
    max_capital_per_asset_class = 0.60;
    max_daily_drawdown = 0.05;
    max_intraday_loss = 0.03;
    kelly_cap = 0.20;
    volatility_target = 0.15;
    circuit_breaker_enabled = true;
  }

let default =
  {
    mode = Paper;
    initial_capital = 100_000.;
    venues = [ { venue = Paper; api_key = None; api_secret = None; base_url = None } ];
    strategies =
      [
        {
          id = "sma_btc";
          name = "sma";
          enabled = true;
          weight = 1.0;
          instruments = [ { symbol = "BTC-USD"; asset_class = Crypto; venue_hint = Some Paper } ];
          params = [ ("fast", 20.); ("slow", 50.) ];
        };
      ];
    risk = default_risk;
    interfaces = { web_port = 8080; telegram_token = None };
  }

let strip s = String.trim s

let parse_bool s =
  match String.lowercase_ascii (strip s) with
  | "true" | "yes" | "1" -> Some true
  | "false" | "no" | "0" -> Some false
  | _ -> None

let parse_float_opt s =
  try Some (float_of_string (strip s)) with _ -> None

let parse_int_opt s =
  try Some (int_of_string (strip s)) with _ -> None

let parse_kv_line line =
  match String.split_on_char ':' line with
  | [ k; v ] -> Some (String.lowercase_ascii (strip k), strip v)
  | _ -> None

let parse_instrument symbol =
  let asset_class =
    if String.contains symbol '-' then Crypto
    else if String.length symbol <= 5 then Equity
    else Crypto
  in
  { symbol; asset_class; venue_hint = None }

let load path =
  let lines =
    try
      let ic = open_in path in
      let rec loop acc =
        match input_line ic with
        | line -> loop (line :: acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      Ok (loop [])
    with Sys_error msg -> Error (Config_error msg)
  in
  match lines with
  | Error e -> Error e
  | Ok raw_lines ->
      let filtered =
        raw_lines
        |> List.map strip
        |> List.filter (fun l -> l <> "" && l.[0] <> '#')
      in
      let cfg = ref default in
      let set f = cfg := f !cfg in
      List.iter
        (fun line ->
          match parse_kv_line line with
          | None -> ()
          | Some (k, v) -> (
              match k with
              | "mode" -> (
                  match mode_of_string (String.lowercase_ascii v) with
                  | Ok m -> set (fun c -> { c with mode = m })
                  | Error _ -> ())
              | "initial_capital" -> (
                  match parse_float_opt v with
                  | Some x -> set (fun c -> { c with initial_capital = x })
                  | None -> ())
              | "web_port" -> (
                  match parse_int_opt v with
                  | Some p ->
                      set (fun c -> { c with interfaces = { c.interfaces with web_port = p } })
                  | None -> ())
              | "telegram_token" ->
                  set (fun c -> { c with interfaces = { c.interfaces with telegram_token = Some v } })
              | "venues" ->
                  let vs = String.split_on_char ',' v |> List.map strip in
                  let parsed =
                    vs
                    |> List.filter_map (fun name ->
                           match venue_of_string (String.lowercase_ascii name) with
                           | Ok venue ->
                               Some { venue; api_key = None; api_secret = None; base_url = None }
                           | Error _ -> None)
                  in
                  if parsed <> [] then set (fun c -> { c with venues = parsed })
              | "max_position_size" -> (
                  match parse_float_opt v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with max_position_size = x } })
                  | None -> ())
              | "max_capital_per_asset_class" -> (
                  match parse_float_opt v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with max_capital_per_asset_class = x } })
                  | None -> ())
              | "max_daily_drawdown" -> (
                  match parse_float_opt v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with max_daily_drawdown = x } })
                  | None -> ())
              | "max_intraday_loss" -> (
                  match parse_float_opt v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with max_intraday_loss = x } })
                  | None -> ())
              | "kelly_cap" -> (
                  match parse_float_opt v with
                  | Some x -> set (fun c -> { c with risk = { c.risk with kelly_cap = x } })
                  | None -> ())
              | "volatility_target" -> (
                  match parse_float_opt v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with volatility_target = x } })
                  | None -> ())
              | "circuit_breaker_enabled" -> (
                  match parse_bool v with
                  | Some x ->
                      set (fun c -> { c with risk = { c.risk with circuit_breaker_enabled = x } })
                  | None -> ())
              | "strategy_symbols" ->
                  let symbols = String.split_on_char ',' v |> List.map strip |> List.filter (( <> ) "") in
                  if symbols <> [] then
                    let instruments = List.map parse_instrument symbols in
                    set (fun c ->
                        {
                          c with
                          strategies =
                            [
                              {
                                id = "default";
                                name = "sma";
                                enabled = true;
                                weight = 1.0;
                                instruments;
                                params = [ ("fast", 20.); ("slow", 50.) ];
                              };
                            ];
                        })
              | _ -> ()))
        filtered;
      Ok !cfg
