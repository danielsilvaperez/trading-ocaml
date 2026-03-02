open Trading_core

type control_command =
  | Enable_strategy of string
  | Disable_strategy of string
  | Replace_strategy of Config.strategy_instance
  | Get_status
  | Get_portfolio
  | Trigger_kill_switch

type status = {
  mode : Config.execution_mode;
  running : bool;
  enabled_strategies : string list;
  breaker_tripped : bool;
}

type bus_message =
  | Control of control_command
  | RiskAlert of Types.risk_violation
  | Audit of string
