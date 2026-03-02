open Trading_core

let html_page ~title body =
  Printf.sprintf
    "<!doctype html><html><head><meta charset='utf-8'><title>%s</title><style>body{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;background:#f6f7fb;color:#101420;margin:2rem;} .grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:1rem;} .card{background:white;padding:1rem;border-radius:10px;box-shadow:0 1px 3px rgba(0,0,0,.08);} table{width:100%%;border-collapse:collapse;} td,th{padding:6px;border-bottom:1px solid #e6e8ef;text-align:left;} .warn{color:#b42318;font-weight:700;}</style></head><body><h1>%s</h1>%s</body></html>"
    title title body

let render_dashboard engine =
  let status = Trading_engine.Engine.status engine in
  let snap = Trading_engine.Engine.portfolio_snapshot engine in
  let exposure_rows =
    snap.exposure_by_asset
    |> List.map (fun (asset, v) ->
           let name = match asset with Types.Equity -> "Equity" | Types.Crypto -> "Crypto" in
           Printf.sprintf "<tr><td>%s</td><td>%.2f</td></tr>" name v)
    |> String.concat ""
  in
  let position_rows =
    snap.portfolio.positions
    |> List.map (fun (p : Types.position) ->
           Printf.sprintf "<tr><td>%s</td><td>%.4f</td><td>%.2f</td><td>%.2f</td></tr>" p.instrument.symbol
             p.quantity p.market_price p.unrealized_pnl)
    |> String.concat ""
  in
  let risk_banner =
    if status.breaker_tripped then "<p class='warn'>CIRCUIT BREAKER TRIPPED</p>" else ""
  in
  html_page ~title:"Trading Engine Dashboard"
    (Printf.sprintf
       "%s<div class='grid'>
       <div class='card'><h3>Portfolio</h3><p>Equity: %.2f</p><p>Realized PnL: %.2f</p><p>Unrealized PnL: %.2f</p><p>Drawdown: %.2f%%</p><p>Sharpe: %.3f</p><p>Sortino: %.3f</p></div>
       <div class='card'><h3>Engine Status</h3><p>Running: %b</p><p>Mode: %s</p><p>Enabled Strategies: %d</p></div>
       <div class='card'><h3>Exposure by Asset Class</h3><table><tr><th>Asset</th><th>Exposure</th></tr>%s</table></div>
       </div>
       <div class='card'><h3>Live Positions</h3><table><tr><th>Symbol</th><th>Qty</th><th>Price</th><th>Unrealized PnL</th></tr>%s</table></div>"
       risk_banner snap.portfolio.equity snap.realized_pnl snap.unrealized_pnl (snap.drawdown *. 100.)
       snap.sharpe snap.sortino status.running
       (match status.mode with
       | Config.Live -> "live"
       | Config.Paper -> "paper"
       | Config.Backtest -> "backtest"
       | Config.Simulation -> "simulation")
       (List.length status.enabled_strategies)
       exposure_rows position_rows)

let run ~engine ~port =
  let app =
    Dream.logger
    @@ Dream.router
         [
           Dream.get "/" (fun _req -> Dream.html (render_dashboard engine));
           Dream.get "/status" (fun _req ->
               let s = Trading_engine.Engine.status engine in
               Dream.respond
                 (Printf.sprintf "running=%b strategies=%d breaker=%b" s.running
                    (List.length s.enabled_strategies) s.breaker_tripped));
           Dream.get "/metrics" (fun _req ->
               let snap = Trading_engine.Engine.portfolio_snapshot engine in
               let metrics =
                 Printf.sprintf
                   "engine_equity %.10f\nengine_drawdown %.10f\nengine_realized_pnl %.10f\nengine_unrealized_pnl %.10f"
                   snap.portfolio.equity snap.drawdown snap.realized_pnl snap.unrealized_pnl
               in
               Dream.respond ~headers:[ ("Content-Type", "text/plain; version=0.0.4") ] metrics);
         ]
  in
  Dream.serve ~port app
