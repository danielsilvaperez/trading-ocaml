#!/usr/bin/env bash
set -euo pipefail

echo "== status =="
dune exec trading-ocaml -- status -c config.yaml

echo "== backtest =="
dune exec trading-ocaml -- backtest -c config.yaml --strategy sma --symbol BTC-USD --from 2022-01-01 --to 2023-01-01 --csv data/BTC-USD.csv

echo "== simulate =="
dune exec trading-ocaml -- simulate --paths 500 --horizon 252 --ruin-threshold 0.7

echo "== run (web) =="
echo "dune exec trading-ocaml -- run -c config.yaml --web --port 8080"
