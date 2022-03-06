open Ast
open Astutil

exception SyntaxError of string
exception IOError of string
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
  e

(* 
  Given a unit test set and AST, check if AST passes tests
  Input: 
    - test_set : a list of tests of testType with inputs and their corresponding output
    - code : the code to be evaluated upon
  Output:
    true, if code passes all tests
    false, otherwise
*)
let rec run_unit_tests (test_set : testType list) (code : Expr.t) : bool =
  let run_test (test: testType) (code : Expr.t) : bool =
    (* Assume code is a function in an ELet (_, EFun/EFix (_ , _), EHole) *)
    match code with
      | Expr.ELet (id, f, Expr.EHole) -> 
        begin match f with
          | EFun (_, _) | EFix (_, _) -> 
            let (test_input, test_output) = test in
            let output = eval (Expr.ELet (id, f, EBinOp(EVar id, Expr.OpAp, EInt test_input))) in
            begin match output with
              | VInt n -> n = test_output
              | _ -> false
            end
          | _ -> raise (SyntaxError "Code is not a function.")
        end
      | _ -> raise (SyntaxError "Code is not a function in ELet (_, _, EHole) form")
  in
  match test_set with
    | [] -> true
    | hd :: tl -> if run_test hd code then run_unit_tests tl code else false

(* Given an assignment number, load the code and test data
  Input: 
    - assignment : index of assignment
  Output:
    (codes, tests) : 
      - codes : the codes (training data) for the assignment
      - tests : the unit tests for the assignment
*)
let load_assignment (directory : string) (assignment : int) (num_files : int) : (Expr.t list * testType list) =
  let rec load_assignment_aux (assignment : int) (index : int) : Expr.t list = 
    if index < 0 then [] else
      let filename = directory ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index ^ ".ml" in
      let code = parse_file filename in
      code :: (load_assignment_aux assignment (index - 1))
  in
  let filename = directory  ^ "/" ^ string_of_int assignment ^ "/" ^ "tests.ml" in
  let tests_cons = parse_file filename in
  let rec combine_tests (tests_cons : Expr.t) : (testType list) = 
    match tests_cons with
      | EBinOp (EPair (EInt a, EInt b), OpCon, ENil) -> [(a, b)]
      | EBinOp (EPair (EInt a, EInt b), OpCon, tl) -> (a, b) :: combine_tests tl
      | _ -> raise (IOError "Test file in incorrect format.")
  in 
  (load_assignment_aux assignment (num_files - 1), combine_tests tests_cons)