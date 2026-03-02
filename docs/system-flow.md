# System Flow

1. `config.yaml` is loaded into strongly typed `Config.t`.
2. `Engine.create` builds strategy manager, risk manager, portfolio state, observability, and control bus.
3. Venue streams emit typed `market_event` values asynchronously.
4. Strategy manager fans out each event to enabled strategy instances (state per strategy per instrument).
5. Signal aggregation policy resolves independent, ensemble-vote, or risk-parity outputs.
6. Order router converts signals to orders and enforces risk rules before routing to venue modules.
7. Fills update portfolio accounting and performance metrics.
8. Web/CLI/Telegram interfaces query or mutate engine state through typed commands on the internal message bus.
