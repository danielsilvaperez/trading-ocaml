open Trading_core

type policy = Independent | EnsembleVoting | RiskParity

val resolve :
  policy ->
  weights:(string * float) list ->
  Types.signal list ->
  Types.signal list
