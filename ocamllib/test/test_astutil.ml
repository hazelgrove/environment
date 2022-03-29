open OUnit2
open Astutil
open Ast

(* Test change_ast *)


(* Test eval *)
let test_int = fun _ -> assert_equal (Value.VInt 1) (eval (Expr.EInt 1) 100)

(* Run *)
let tests = 
  "Testing eval" >::: 
  [
    "Int"  >:: test_int;
  ];;
