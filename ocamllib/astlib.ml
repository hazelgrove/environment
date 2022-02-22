open Ast
open Astutil

exception SyntaxError of string
exception TestingError of string
exception NotImplemented of unit

type testType = (int * int)

(* 
    Given an zippered AST, apply the action 
    Input: 
      - e : an AST with cursor (TODO: implement zast)
      - action : action applied to e
    Output:
      the modified AST
*)
let change_ast (e : Expr.t) (action : Action.t) : Expr.t =
  raise (NotImplemented ())

(* 
  Given a unit test set and AST, check if AST passes tests
  Input: 
    - test_set : a list of tests of testType with inputs and their corresponding output
    - code : the code to be evaluated upon
  Output:
    true, if code passes all tests
    false, otherwise
*)
let rec run_unit_testset (test_set : testType list) (code : Expr.t) : bool =
  let run_test (test: testType) (code : Expr.t) : bool =
    (* Assume code is a function in an ELet (_, _, EHole) *)
    match code with
      | Expr.ELet (id, f, Expr.EHole) -> 
        let (test_input, test_output) = test in
        let output = eval (Expr.ELet (id, f, EBinOp(EVar id, Expr.OpAp, EInt test_input))) in
        begin match output with
          | VInt n -> n = test_output
          | _ -> false
        end
      | _ -> raise (SyntaxError "Code is not a function in ELet (_, _, EHole) form")
  in
  match test_set with
    | [] -> true
    | hd :: tl -> if run_test hd code then run_unit_testset tl code else false

let load_assignment (test_num : int) : Expr.t =
  raise (NotImplemented ())