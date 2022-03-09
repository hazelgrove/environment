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

let edge_to_list (arr: (int, int_elt, c_layout) Array1.t) (len : int) : (int * int * int) list = 
  let rec edge_to_list_aux (arr: (int, int_elt, c_layout) Array1.t) (len : int) (l : (int * int) list) : (int * int) list =
    edge_to_list_aux arr (len - 1) ((arr.{len - 1, 0}, arr.{len - 1, 1}, arr.{len - 1, 2}) :: list)
  in
  edge_to_list_aux arr len []

let change_ast_c (action : Action.t) : unit =
  let nodes = get_nodes () in
  let edges = get_edges () in
  let e = c_to_expr nodes in
  let change_ast (e : Expr.t) (action : Action.t) : Expr.t =
    EBool true
  in
  let (nodes, edges) = expr_to_c (change_ast e action) in 
  pass_nodes nodes;;
  pass_edges edges

let load_assignment_c (test_num : int) : unit =
  let load_assignment (test_num : int) : Expr.t =
    EBool true
  in
  let (nodes, edges) = expr_to_c (load_assignment test_num) in 
  pass_nodes nodes;;
  pass_edges edges

(* let change_node (action : int) : unit =
  let nodes = get_nodes () in
  if nodes.{action} = 0 then 
    (* let () = Array1.set nodes action 1 in pass_nodes nodes *)
    pass_nodes nodes
  else
    (* let () = Array1.set nodes action 0 in pass_nodes nodes *)
    pass_nodes nodes *)

let select_root :Ast.Expr.t -> Ast.Expr.z_t = 
  (* Convert an unzipped ast into a zipped one, by selecting the root*)
  (function tree -> Ast.Expr.Cursor tree) 

let rec unzip_ast  (tree : Ast.Expr.z_t) : Ast.Expr.t = 
  match tree with 
  | Cursor arg -> arg 
  | EUnop_L (unop, l_child) -> EUnop (unop, unzip_ast l_child)
  | EBinop_L (l_child, binop,r_child) -> BinOp (unzip_ast l_child, binop,r_child)
  | EBinop_R (l_child, binop,r_child) -> BinOp (l_child, binop,unzip_ast r_child)
  | ELet_L (var,l_child, r_child) -> ELet (var, unzip_ast l_child, r_child)
  | ELet_R (var,l_child, r_child) -> ELet (var, l_child, unzip_ast r_child)
  | EIf_L (l, c, r) -> EIf(unzip_ast l, c,r)
  | EIf_C (l, c, r) -> EIf(l, unzip_ast c, r) 
  | EIf_R (l, c, r) -> EIf(l, c, unzip_ast r) 
  | EFun_L (var, child) -> EFun(var, unzip_ast child) 
  | EFix_L (var, child) -> EFix(var, unzip_ast child) 

let perform : Ast.Action.t -> Ast.Expr.z_t -> Ast.Expr.z_t =
  (function action -> 
    let rec act_on tree = (
      match action with 
      | Construct shape -> 
        (* ( match tree with 
        | EUnop_L (op,r_child) -> UnopL (op,act_on r_child) 
        | EBinop_L (l_child, op, r_child) -> EBinop_L (act_on l_child, op, r_child)
        | EBinop_R (l_child, op, r_child) -> EBinop_R (l_child, op, act_on r_child)
        | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
        | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)
        | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
        | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
        | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
        | EFun_L (var, child) -> Efun_L (var, act_on child)
        | EFix_L (var, child) -> Efun_R (var, act_on child)
        | Cursor subtree ->  (
            match subtree with 
            | EHole -> (
              match shape with 
              | Arrow funcName -> Cursor(Efun (funcName,EHole))
              | Num   numVal   -> Cursor(EInt numVal)
              | Asc
            )
          )
        ) *)
        (*for now hold of on constructing*)
        tree 
      | Move Child n -> 
        (match tree with 
        | EUnop_L (op,r_child) -> UnopL (op,act_on r_child) 
        | EBinop_L (l_child, op, r_child) -> EBinop_L (act_on l_child, op, r_child)
        | EBinop_R (l_child, op, r_child) -> EBinop_R (l_child, op, act_on r_child)
        | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
        | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)
        | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
        | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
        | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
        | EFun_L (var, child) -> Efun_L (var, act_on child)
        | EFix_L (var, child) -> Efun_R (var, act_on child)
        | Cursor subtree -> (
          match n with 
          | 0 -> (
            match subtree with
              | EUnOp  (op,arg) -> EUnop_L (op, Cursor (arg))
              | EBinop (arg_l, op, arg_r) -> EBinop_L (Cursor (arg_l), op, arg_r)
              | ELet (varn, arg_l, arg_r) -> ELet_L (varn, Cursor(arg_l),arg_r)
              | EIf (arg_l, arg_c,arg_r) -> EIf_L (Cursor (arg_l), arg_c,arg_r)
              | EFun (varn, arg_l) -> EFun_L (varn, Cursor (arg_l))
              | EFix (varn, arg_l) -> EFix (varn, Cursor (arg_l))
              | _ -> tree  (*all invalid actions are noops*)
            ) 
          | 1 ->( 
            match subtree with 
            | EBinop (arg_l, op, arg_r) -> EBinop_R (arg_l, op, Cursor(arg_r))
            | ELet (varn, arg_l, arg_r) -> ELet_R (varn, arg_l,Cursor(arg_r))
            | EIf (arg_l, arg_c,arg_r) -> EIf_C (arg_l, Cursor(arg_c),arg_r)
            | _ -> tree  (*all invalid actions are noops*)
            )
          | 1 -> (
            match subtree with 
            | EIf (arg_l, arg_c,arg_r) -> EIf_R (arg_l, arg_c,Cursor(arg_r))
            )
        )
      )
    | Parent -> (
      match tree with 
      | EUnop_L (op, Cursor arg ) -> Cursor (Unop (op, arg))
      | EUnop_L (op, arg) -> EUnop_L (op, act_on arg) 

      | EBinop_L (Cursor arg, op, r_child) -> Cursor (BinOp (arg, op, r_child))
      | EBinop_L (l_child, op, r_child) -> EBinop_L (act_on l_child, op, r_child)
      | EBinop_R (l_child, op, Cursor arg) -> Cursor (BinOp (l_child, op, arg))
      | EBinop_R (l_child, op, r_child) -> EBinop_R (l_child, op, act_on r_child)
      
      | ELet_L (var,Cursor arg, r_child )  -> Cursor (ELet (var,arg, r_child))
      | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
      | ELet_R (var,l_child, Cursor arg )  -> Cursor (ELet (var,l_child,act_on arg))
      | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)

      | EIf_L (Cursor arg, c, r) -> Cursor (EIf (arg, c,r))
      | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
      | EIf_C (l, Cursor arg, r) -> Cursor (EIf (l, arg, r))
      | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
      | EIf_R (l, c, Cursor arg) -> Cursor (EIf (l, c, arg))
      | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)

      | EFun_L (var, Cursor arg) ->  Cursor (Efun (var, arg))
      | EFun_L (var, child) -> Efun_L (var, act_on child)
      | EFix_L (var, Cursor arg) -> Cursor (Efun (var, arg))
      | EFix_L (var, child) -> Efun_R (var, act_on child)
      | -> tree
      )
    ) in act_on 
  )


(*
let _ = Callback.register "evaluate_ast" evaluate_ast
let _ = Callback.register "change_node" change_node_c *)
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
