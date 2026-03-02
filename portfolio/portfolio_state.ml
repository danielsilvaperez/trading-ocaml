open Trading_core

type t = {
  mutable portfolio : Types.portfolio;
  mutable equity_curve : float list;
}

type snapshot = {
  portfolio : Types.portfolio;
  realized_pnl : float;
  unrealized_pnl : float;
  twr : float;
  drawdown : float;
  sharpe : float;
  sortino : float;
  exposure_by_asset : (Types.asset_class * float) list;
}

let create ~capital =
  let p = Accounting.empty ~capital in
  { portfolio = p; equity_curve = [ p.equity ] }

let push_equity (t : t) = t.equity_curve <- t.equity_curve @ [ t.portfolio.equity ]

let apply_trade (t : t) tr =
  t.portfolio <- Accounting.apply_trade t.portfolio tr;
  push_equity t

let mark_to_market (t : t) instrument ~price =
  t.portfolio <- Accounting.mark_to_market t.portfolio instrument ~price;
  push_equity t

let snapshot (t : t) =
  let realized = List.fold_left (fun acc p -> acc +. p.Types.realized_pnl) 0. t.portfolio.positions in
  let unrealized =
    List.fold_left (fun acc p -> acc +. p.Types.unrealized_pnl) 0. t.portfolio.positions
  in
  let returns =
    let rec loop acc = function
      | a :: (b :: _ as rest) when a > 0. -> loop (((b /. a) -. 1.) :: acc) rest
      | _ :: rest -> loop acc rest
      | [] -> List.rev acc
    in
    loop [] t.equity_curve
  in
  {
    portfolio = t.portfolio;
    realized_pnl = realized;
    unrealized_pnl = unrealized;
    twr = Metrics.time_weighted_return ~equity_curve:t.equity_curve;
    drawdown = Metrics.max_drawdown ~equity_curve:t.equity_curve;
    sharpe = Metrics.sharpe_ratio ~returns ~risk_free_rate:0.0;
    sortino = Metrics.sortino_ratio ~returns ~risk_free_rate:0.0;
    exposure_by_asset = Accounting.exposure_by_asset_class t.portfolio;
  }

let equity_curve (t : t) = t.equity_curve
