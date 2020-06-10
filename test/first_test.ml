open OUnit2
open Problems

let tests = "Tests for first 10 problems" >::: [
 "last some" >:: (
  fun _ -> assert_equal (Some "d") (First_ten.last [ "a"; "b"; "c"; "d"
  ]));
]


let _ = run_test_tt_main tests
