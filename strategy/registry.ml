open Trading_core

let make cfg =
  let lname = String.lowercase_ascii cfg.Config.name in
  match lname with
  | "sma" | "sma_crossover" ->
      Ok (Strategy_pack.create (module Sma_crossover.M) cfg)
  | "mean_reversion" ->
      Ok (Strategy_pack.create (module Mean_reversion.M) cfg)
  | "momentum_breakout" ->
      Ok (Strategy_pack.create (module Momentum_breakout.M) cfg)
  | "vwap_deviation" ->
      Ok (Strategy_pack.create (module Vwap_deviation.M) cfg)
  | "volatility_compression_breakout" ->
      Ok (Strategy_pack.create (module Volatility_compression_breakout.M) cfg)
  | _ -> Error (Types.Strategy_error ("Unknown strategy: " ^ cfg.name))
