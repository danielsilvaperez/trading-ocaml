open Trading_core

val load_bars :
  instrument:Types.instrument ->
  path:string ->
  (Types.market_event list, Types.error) result
