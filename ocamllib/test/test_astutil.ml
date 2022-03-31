open OUnit2
open Astutil
open Ast
open Ast.Expr
open Utils

(* TODO: Test unzip_ast *)
let test_unzip_ast = fun _ ->
  let e = parse "let x = 2 in 2 + x / 3" in
  assert_equal (unzip_ast (Cursor e)) e

(* TODO: Test subst *)


(* TODO: Test parse *)
let test_parse = fun _ -> 
  let parse_map = List.map (fun (x, y) -> (parse x, y)) in
  let tests = [
    (* Vars *)
    ("x", EVar "x");
    ("abs", EVar "abs");

    (* Ints *)
    ("3", EInt 3);
    ("-1", EInt (-1));

    (* Bools *)
    ("true", EBool true);
    ("false", EBool false);
  ] in
  let tests = parse_map tests in
  test_set tests ( = )

(* TODO: Test eval *)
let test_eval = fun _ ->
  let eval_map = List.map (fun (x, y) -> (eval x 100, y)) in
  let tests = [
    (* Ints *)
    (Expr.EInt 32, Value.VInt 32);
    (Expr.EInt (-5), Value.VInt (-5));
    (Expr.EInt 0, Value.VInt 0);

    (* Bools *)
    (Expr.EBool true, Value.VBool true);
    (Expr.EBool false, Value.VBool false);

    (* Functions *)
    (parse "fun x -> 2 * x + 1", Value.VFun ("x", parse "2 * x + 1"));
    (parse "fun a -> a 2", Value.VFun ("a", parse "a 2"));
  ] in
  let tests = eval_map tests in
  test_set tests ( = );

  let eval_error_map = List.map (fun (x, y) -> ((fun () -> eval x 10), y)) in
  let tests = [
    (parse "let rec f n = n * f (n - 1) in f 1", RuntimeError "Stack overflow");
  ] in
  let tests = eval_error_map tests in 
  test_exception_set tests

(* TODO: Test c_to_expr *)


(* TODO: Test expr_to_c *)

(* Run *)
let unit_tests = 
  "Testing astutil.ml..." >::: 
  [
    "unzip_ast" >:: test_unzip_ast;
    "parse"     >:: test_parse;
    "eval"      >:: test_eval;
  ];;
