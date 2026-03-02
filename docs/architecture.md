# Architecture Diagram

```text
                          +-------------------------+
                          |     Interfaces Layer    |
                          | CLI | Web | Telegram    |
                          +------------+------------+
                                       |
                                       v
+-------------------+    +-------------------------+    +-------------------+
|   Config & Obs    +--->+      Engine Core        +--->+   Venue Adapters  |
| YAML/TOML-like    |    | Strategy/Risk/Router    |    | Robinhood/Coinbase|
| Logs/Metrics/Audit|    | Async Orchestrator      |    | Alpaca/Binance    |
+-------------------+    +------+------------------+    +---------+---------+
                                |                                 |
                                v                                 v
                      +---------+---------+            +----------+---------+
                      | Strategy Modules  |            | Market Data Streams|
                      | SMA/MR/MOMO/VWAP  |            | Async Lwt streams  |
                      +---------+---------+            +--------------------+
                                |
                                v
                      +---------+---------+
                      | Risk & Portfolio  |
                      | Rules + Accounting|
                      +---------+---------+
                                |
                 +--------------+--------------+
                 v                             v
       +---------+----------+         +--------+-----------+
       | Backtest Engine    |         | Monte Carlo Engine |
       | Replay + Frictions |         | Path simulations   |
       +--------------------+         +--------------------+
```
