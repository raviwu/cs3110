open OUnit2

let ae exp got _test_ctxt = assert_equal exp got

let tests = [
  "Example" >:: ae true true
]

let () =
  run_test_tt_main ("Chapter X tests" >::: tests)
