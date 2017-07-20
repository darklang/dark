open OUnit2

let test1 test_ctxt = assert_equal 1 1
let test2 test_ctxt = assert_equal 2 2


let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2]


let () =
  run_test_tt_main suite
