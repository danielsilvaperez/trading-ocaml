open Trading_core

type execution_model = {
  base_slippage_bps : float;
  commission_bps : float;
  latency_ms : int;
}

type summary = {
  start_equity : float;
  end_equity : float;
  total_return : float;
  max_drawdown : float;
  sharpe : float;
  sortino : float;
  trades : int;
}

val run :
  config:Config.t ->
  csv_path:string ->
  instrument:Types.instrument ->
  execution_model:execution_model ->
  (summary, Types.error) result
