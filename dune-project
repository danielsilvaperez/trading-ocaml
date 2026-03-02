(lang dune 3.13)
(name trading_ocaml)
(generate_opam_files true)

(package
 (name trading_ocaml)
 (synopsis "Production-grade multi-asset trading engine in OCaml")
 (description "Modular institutional-style trading platform supporting live, paper, backtest, and simulation modes")
 (depends
  ocaml
  dune
  lwt
  lwt_ppx
  cmdliner
  dream
  logs
  fmt
  ptime
  qcheck
  alcotest))
