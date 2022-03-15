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
  ELet("y", EFun ("x", parse "x + 1"), EHole)

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
          | _ -> false
        end
      | _ -> false
  in
  match test_set with
    | [] -> true
    | hd :: tl -> if run_test hd code then run_unit_tests tl code else false

(* Given an assignment number, load the unit tests
  Input: 
    - assignment : index of assignment
  Output:
    (codes, tests) : 
      - codes : the codes (training data) for the assignment
      - tests : the unit tests for the assignment
*)
let load_tests (directory : string) (assignment : int) : testType list =
  let filename = directory  ^ "/" ^ string_of_int assignment ^ "/test.ml" in
  let tests_cons = parse_file filename in
  let rec combine_tests (tests_cons : Expr.t) : (testType list) = 
    match tests_cons with
      | EBinOp (EPair (EInt a, EInt b), OpCon, ENil) -> [(a, b)]
      | EBinOp (EPair (EInt a, EInt b), OpCon, tl) -> (a, b) :: combine_tests tl
      | _ -> raise (IOError "Test file in incorrect format.")
  in 
  combine_tests tests_cons

(* Given an assignment number, load the code and test data
  Input: 
    - assignment : index of assignment
  Output:
    (codes, tests) : 
      - codes : the codes (training data) for the assignment
      - tests : the unit tests for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int) : Expr.t =
  let filename = directory  ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index ^ ".ml" in
  parse_file filename


let rec code_to_string (e : Expr.t) : string = 
  match e with
    | EVar x -> x ^ " "
    | EInt n -> string_of_int n ^ " "
    | EBool b -> string_of_bool b ^ " "
    | EUnOp (_, e) -> "(-" ^ code_to_string e ^ ") "
    | EBinOp (e1, op, e2) -> 
      let op_string = begin match op with
        | OpPlus -> "+"
        | OpMinus -> "-"
        | OpTimes -> "*"
        | OpDiv -> "/"
        | OpLt -> "<"
        | OpLe -> "<="
        | OpGt -> ">"
        | OpGe -> ">="
        | OpEq -> "="
        | OpNe -> "!="
        | OpCon -> "::"
        | OpAp -> " "
      end
      in
      "(" ^ code_to_string e1 ^ " " ^ op_string ^ " " ^ code_to_string e2  ^ ") "
    | EIf (cond, e1, e2) -> "(if " ^ code_to_string cond ^ " then " ^ code_to_string e1 ^ " else " ^ code_to_string e2 ^ ") "
    | ELet (x, EFix (_, e1), EHole) -> "let rec " ^ x ^ resolve_fun e1 ^ " "
    | ELet (x, EFix (_, e1), e2) -> "let rec " ^ x ^ resolve_fun e1 ^ " in " ^ code_to_string e2 ^ " "
    | ELet (x, EFun (arg, e1), EHole) -> "let " ^ x ^ resolve_fun (EFun (arg, e1)) ^ " "
    | ELet (x, EFun (arg, e1), e2) -> "let " ^ x ^ resolve_fun (EFun (arg, e1)) ^ " in " ^ code_to_string e2 ^ " "
    | ELet (x, e1, EHole) -> "let " ^ x ^ " = " ^ code_to_string e1 ^ " "
    | ELet (x, e1, e2) -> "let " ^ x ^ " = " ^ code_to_string e1 ^ " in " ^ code_to_string e2 ^ " "
    | EFix (_, _) -> raise (SyntaxError "Incorrect syntax with fix")
    | EFun (x, e) -> "(fun " ^ x ^ " -> " ^ code_to_string e ^ ") "
    | EPair (e1, e2) -> "(" ^ code_to_string e1 ^ ", " ^ code_to_string e2 ^ ") "
    | EHole -> "<HOLE> "
    | ENil -> "[] "
and resolve_fun (e : Expr.t) : string =
  match e with 
    | EFun (x, e) -> " " ^ x ^ resolve_fun e
    | _ -> " = " ^ code_to_string e  ^ " "
