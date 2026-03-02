# Design Rationale

- Pure domain-first model: strategies and risk operate on ADTs, not JSON blobs.
- Isolation boundaries: exchange HTTP/websocket logic is constrained to `venues/`.
- Composable risk: rule functions are independent and easy to test/property-check.
- State control: no global mutable singletons; state is contained inside engine instances.
- Execution polymorphism: same strategy/risk stack reused for live, paper, backtest, simulation.
- Operational visibility: structured logs, metrics endpoint, audit stream for order lifecycle.
- Extension path: add a venue by implementing `VENUE`; add a strategy by registering `STRATEGY`.
