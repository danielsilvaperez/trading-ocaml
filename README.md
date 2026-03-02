# trading-ocaml

Production-grade multi-asset trading engine scaffold in OCaml with strict modular architecture.

## Highlights

- Strongly typed domain model (`core/`) for asset classes, orders, trades, positions, signals, and risk violations.
- Venue abstraction (`venues/`) with plug-in implementations:
  - `Robinhood_venue`
  - `Coinbase_venue`
  - `Alpaca_venue`
  - `Binance_venue`
  - `Paper_venue`
- Strategy framework (`strategy/`) with institutional-style extensibility:
  - SMA crossover
  - Mean reversion
  - Momentum breakout
  - VWAP deviation
  - Volatility compression breakout
- Multi-strategy orchestration (`engine/`) with concurrent streams and aggregation policies:
  - independent
  - ensemble voting
  - risk parity weighting
- Composable risk layer (`risk/`) with:
  - max position size
  - max capital per asset class
  - daily drawdown / intraday loss controls
  - volatility-adjusted sizing
  - Kelly cap
  - circuit breaker / kill switch
  - exposure netting checks
- Portfolio and accounting (`portfolio/`) with PnL, drawdown, Sharpe/Sortino, exposure breakdown.
- Backtesting (`backtest/`) with deterministic CSV replay and execution frictions.
- Monte Carlo simulation (`simulation/`) with bootstrap, perturbation, randomized slippage.
- Interfaces (`interfaces/`):
  - CLI (`Cmdliner`)
  - Web dashboard (`Dream`)
  - Telegram command bridge (REPL shim over internal message bus)
- Observability (`core/observability.ml`): structured logs, metrics rendering, audit trail hooks.

## Repository Layout

- `bin/`
- `core/`
- `venues/`
- `strategy/`
- `risk/`
- `portfolio/`
- `engine/`
- `backtest/`
- `simulation/`
- `interfaces/`
- `test/`
- `docs/`

## Build

```bash
opam install . --deps-only
dune build
dune test
```

## CLI Examples

```bash
# run engine

dune exec trading-ocaml -- run -c config.yaml --web --telegram --port 8080

# backtest

dune exec trading-ocaml -- backtest -c config.yaml --strategy sma --symbol BTC-USD --from 2022-01-01 --to 2023-01-01 --csv data/BTC-USD.csv

# simulation

dune exec trading-ocaml -- simulate --paths 1000 --horizon 252 --ruin-threshold 0.7

# status and portfolio

dune exec trading-ocaml -- status -c config.yaml
dune exec trading-ocaml -- portfolio -c config.yaml

# strategy controls

dune exec trading-ocaml -- strategy enable sma::BTC-USD
dune exec trading-ocaml -- strategy disable sma::BTC-USD
```

## Docs

- `docs/architecture.md`
- `docs/system-flow.md`
- `docs/design-rationale.md`

## Demo

```bash
./scripts/demo.sh
```
