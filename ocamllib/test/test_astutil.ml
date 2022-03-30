open OUnit2
open Astutil
open Ast
open Utils

(* TODO: Test unzip_ast *)
let test_cursor = fun _ ->
  let e = parse "let x = 2 in 2 + x / 3" in
  assert_equal (unzip_ast (Cursor e)) e

let test_unzip_ast = fun _ ->
  test_cursor ()

(* TODO: Test subst *)


(* TODO: Test parse *)


(* TODO: Test eval *)
let eval_map = List.map (fun (x, y) -> (eval x 100, y))

let test_eval_int = fun _ -> 
  let tests = [
    (Expr.EInt 32, Value.VInt 32);
    (Expr.EInt (-5), Value.VInt (-5));
    (Expr.EInt 0, Value.VInt 0);
  ] in
  let tests = eval_map tests in
  test_set tests ( = )

let test_eval_bool = fun _ -> 
  let tests = [
    (Expr.EBool true, Value.VBool true);
    (Expr.EBool false, Value.VBool false);
  ] in
  let tests = eval_map tests in
  test_set tests ( = )

let test_eval_fun = fun _ -> 
  let tests = [
    (parse "fun x -> 2 * x + 1", Value.VFun ("x", parse "2 * x + 1"));
    (parse "fun a -> a 2", Value.VFun ("a", parse "a 2"));
  ] in
  let tests = eval_map tests in
  test_set tests ( = )

let test_eval = fun _ ->
  test_eval_int ();
  test_eval_bool ();
  test_eval_fun ()

(* TODO: Test c_to_expr *)


(* TODO: Test expr_to_c *)

(* Run *)
let unit_tests = 
  "Testing asttuil.ml..." >::: 
  [
    "unzip_ast" >:: test_unzip_ast;
    "eval"      >:: test_eval;
  ];;
