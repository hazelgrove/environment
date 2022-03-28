open OUnit2
open Astlib
open Ast
open Astutil

(* Test change_ast *)


(* Test run_unit_tests *)
let test_empty_code = fun _ -> 
  let test_set = [(1, 2); (2, 3); (-2, -1)] in
  let code = Expr.EHole in
  assert_equal false (run_unit_tests test_set code)

let test_empty_tests = fun _ -> 
  let test_set = [] in
  let code = Expr.EHole in
  assert_equal true (run_unit_tests test_set code)

let test_invalid_code = fun _ -> 
  let test_set = [(1, 2); (2, 3); (-2, -1)] in
  let code = parse "fun x -> x + 1" in
  assert_equal false (run_unit_tests test_set code)

let test_pass_all = fun _ ->
  let test_set = [(1, 2); (2, 3); (-2, -1)] in 
  let code = parse "let y x = x + 1" in
  assert_equal true (run_unit_tests test_set code);

  let test_set = [(1, 2); (2, 4); (-1, -2); (0, 0)] in 
  let code = parse "let y x = x * 2" in
  assert_equal true (run_unit_tests test_set code)

let test_pass_partial = fun _ ->
  let test_set = [(1, 3); (2, 3); (-2, 0)] in 
  let code = parse "let y x = x + 1" in
  assert_equal false (run_unit_tests test_set code);

  let test_set = [(1, 2); (2, 4); (-1, -2); (0, 0)] in 
  let code = parse "let y x = x * 3" in
  assert_equal false (run_unit_tests test_set code)

let test_fail_all = fun _ ->
  let test_set = [(1, 3); (2, 4); (-2, 0)] in 
  let code = parse "let y x = x + 1" in
  assert_equal false (run_unit_tests test_set code);

  let test_set = [(1, 2); (2, 4); (-1, -2); (0, 4)] in 
  let code = parse "let y x = x * 3" in
  assert_equal false (run_unit_tests test_set code)

(* Run *)
let tests = 
  "Testing run_unit_tests" >::: 
  [
    "Empty Code"  >:: test_empty_code;
    "Empty Tests"  >:: test_empty_tests;
    "Invalid Code"  >:: test_invalid_code;
    "Pass All Tests"  >:: test_pass_all;
    "Pass Part of the Tests"  >:: test_pass_partial;
    "Fail All Tests"  >:: test_fail_all;
  ];;
