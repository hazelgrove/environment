(* Include functions to be used by CInterface *)

open Ast
open Astutil
open Type

exception SyntaxError of string
exception IOError of string
exception NotImplemented of unit

type testType = int * int

(*
     Given an zippered AST, apply the action
     Input:
       - e : an AST with cursor (TODO: implement zast)
       - action : action applied to e
     Output:
       the modified AST
*)
let change_ast (tree : Expr.z_t) (action : Action.t) : Expr.z_t =
  let rec act_on (tree : Expr.z_t) : Expr.z_t =
    match action with
    | Construct shape -> (
        match tree with
        | EUnOp_L (op, r_child) -> EUnOp_L (op, act_on r_child)
        | EBinOp_L (l_child, op, r_child) ->
            EBinOp_L (act_on l_child, op, r_child)
        | EBinOp_R (l_child, op, r_child) ->
            EBinOp_R (l_child, op, act_on r_child)
        | ELet_L (var, l_child, r_child) -> ELet_L (var, act_on l_child, r_child)
        | ELet_R (var, l_child, r_child) -> ELet_R (var, l_child, act_on r_child)
        | EIf_L (l, c, r) -> EIf_L (act_on l, c, r)
        | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
        | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
        | EFun_L (var, typ, child) -> EFun_L (var, typ, act_on child)
        | EFix_L (var, typ, child) -> EFix_L (var, typ, act_on child)
        | EPair_L (l_child, r_child) -> EPair_L (act_on l_child, r_child)
        | EPair_R (l_child, r_child) -> EPair_R (l_child, act_on r_child)
        | Cursor subtree ->
            Cursor
              (match shape with
              | Var varname -> EVar varname
              | Hole -> EHole
              | Nil -> ENil
              | Int value -> EInt value
              | Bool value -> EBool value
              | UnOp op -> EUnOp (op, subtree)
              | BinOp_L op -> EBinOp (subtree, op, EHole)
              | BinOp_R op -> EBinOp (EHole, op, subtree)
              | Let_L varname -> ELet (varname, subtree, EHole)
              | Let_R varname -> ELet (varname, EHole, subtree)
              | If_L -> EIf (subtree, EHole, EHole)
              | If_C -> EIf (EHole, subtree, EHole)
              | If_R -> EIf (EHole, EHole, subtree)
              | Fun (varname, typ) -> EFun (varname, typ, subtree)
              | Fix (varname, typ) -> EFix (varname, typ, subtree)
              | Pair_L -> EPair (subtree, EHole)
              | Pair_R -> EPair (EHole, subtree)))
    | Move (Child n) -> (
        match tree with
        | EUnOp_L (op, r_child) -> EUnOp_L (op, act_on r_child)
        | EBinOp_L (l_child, op, r_child) ->
            EBinOp_L (act_on l_child, op, r_child)
        | EBinOp_R (l_child, op, r_child) ->
            EBinOp_R (l_child, op, act_on r_child)
        | ELet_L (var, l_child, r_child) -> ELet_L (var, act_on l_child, r_child)
        | ELet_R (var, l_child, r_child) -> ELet_R (var, l_child, act_on r_child)
        | EIf_L (l, c, r) -> EIf_L (act_on l, c, r)
        | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
        | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
        | EFun_L (var, typ, child) -> EFun_L (var, typ, act_on child)
        | EFix_L (var, typ, child) -> EFix_L (var, typ, act_on child)
        | EPair_L (l_child, r_child) -> EPair_L (act_on l_child, r_child)
        | EPair_R (l_child, r_child) -> EPair_R (l_child, act_on r_child)
        | Cursor subtree -> (
            match n with
            | 0 -> (
                match subtree with
                | EUnOp (op, arg) -> EUnOp_L (op, Cursor arg)
                | EBinOp (arg_l, op, arg_r) -> EBinOp_L (Cursor arg_l, op, arg_r)
                | ELet (varn, arg_l, arg_r) -> ELet_L (varn, Cursor arg_l, arg_r)
                | EIf (arg_l, arg_c, arg_r) -> EIf_L (Cursor arg_l, arg_c, arg_r)
                | EFun (varname, typ, arg_l) ->
                    EFun_L (varname, typ, Cursor arg_l)
                | EFix (varname, typ, arg_l) ->
                    EFix_L (varname, typ, Cursor arg_l)
                | EPair (arg_l, arg_r) -> EPair_L (Cursor arg_l, arg_r)
                | _ -> tree (*all invalid actions are noops*))
            | 1 -> (
                match subtree with
                | EBinOp (arg_l, op, arg_r) -> EBinOp_R (arg_l, op, Cursor arg_r)
                | ELet (varn, arg_l, arg_r) -> ELet_R (varn, arg_l, Cursor arg_r)
                | EIf (arg_l, arg_c, arg_r) -> EIf_C (arg_l, Cursor arg_c, arg_r)
                | EPair (arg_l, arg_r) -> EPair_R (arg_l, Cursor arg_r)
                | _ -> tree (*all invalid actions are noops*))
            | 2 -> (
                match subtree with
                | EIf (arg_l, arg_c, arg_r) -> EIf_R (arg_l, arg_c, Cursor arg_r)
                | _ -> tree (*all invalid actions are noops*))
            | _ -> tree))
    | Move Parent -> (
        match tree with
        | EUnOp_L (op, Cursor arg) -> Cursor (EUnOp (op, arg))
        | EUnOp_L (op, arg) -> EUnOp_L (op, act_on arg)
        | EBinOp_L (Cursor arg, op, r_child) ->
            Cursor (EBinOp (arg, op, r_child))
        | EBinOp_L (l_child, op, r_child) ->
            EBinOp_L (act_on l_child, op, r_child)
        | EBinOp_R (l_child, op, Cursor arg) ->
            Cursor (EBinOp (l_child, op, arg))
        | EBinOp_R (l_child, op, r_child) ->
            EBinOp_R (l_child, op, act_on r_child)
        (* new: *)
        | EPair_L (Cursor l_child, r_child) -> Cursor (EPair (l_child, r_child))
        | EPair_L (l_child, r_child) -> EPair_L (act_on l_child, r_child)
        | EPair_R (l_child, Cursor r_child) -> Cursor (EPair (l_child, r_child))
        | EPair_R (l_child, r_child) -> EPair_R (l_child, act_on r_child)
        | ELet_L (var, Cursor arg, r_child) -> Cursor (ELet (var, arg, r_child))
        | ELet_L (var, l_child, r_child) -> ELet_L (var, act_on l_child, r_child)
        | ELet_R (var, l_child, Cursor arg) -> Cursor (ELet (var, l_child, arg))
        | ELet_R (var, l_child, r_child) -> ELet_R (var, l_child, act_on r_child)
        | EIf_L (Cursor arg, c, r) -> Cursor (EIf (arg, c, r))
        | EIf_L (l, c, r) -> EIf_L (act_on l, c, r)
        | EIf_C (l, Cursor arg, r) -> Cursor (EIf (l, arg, r))
        | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
        | EIf_R (l, c, Cursor arg) -> Cursor (EIf (l, c, arg))
        | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
        | EFun_L (var, typ, Cursor arg) -> Cursor (EFun (var, typ, arg))
        | EFun_L (var, typ, child) -> EFun_L (var, typ, act_on child)
        | EFix_L (var, typ, Cursor arg) -> Cursor (EFun (var, typ, arg))
        | EFix_L (var, typ, child) -> EFix_L (var, typ, act_on child)
        | _ -> tree)
  in
  act_on tree

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
  let run_test (test : testType) (code : Expr.t) : bool =
    (* Assume code is a function in an ELet (_, EFun/EFix (_ , _), EHole) *)
    match code with
    | Expr.ELet (id, f, Expr.EHole) -> (
        match f with
        | EFun (_, _, _) | EFix (_, _, _) -> (
            let test_input, test_output = test in
            let output =
              try
                eval
                  (Expr.ELet
                     (id, f, EBinOp (EVar id, Expr.OpAp, EInt test_input)))
                  100
              with _ -> VError
            in
            match output with
            | VInt n -> n = test_output
            | VError -> false
            | _ -> false)
        | _ -> false)
    | _ -> false
  in
  match test_set with
  | [] -> true
  | hd :: tl -> if run_test hd code then run_unit_tests tl code else false

(* let possible_actions (expr : Expr.z_t) : Action.avail_actions =
  let rec make_var_arr (i : int) =
    (* create an array of 10 falses *)
    if i < 10 then false :: make_var_arr (i + 1) else []
  in

  let update_var_arr (varname : string) (varlist : bool list) : bool list =
    (* if a variable is in scope, mark its value to true *)
    let rec update_arr (i : int) (l : bool list) : bool list =
      match l with
      | a :: tl ->
          ("v" ^ string_of_int i = varname || a) :: update_arr (i + 1) tl
      | [] -> []
    in
    update_arr 0 varlist
  in

  let acts_init : Action.avail_actions =
    {
      move_parent = (match expr with Cursor _ -> false | _ -> true);
      max_child = 0;
      in_scope = make_var_arr 0;
    }
  in
  (* now finally we recurse *)
  let rec recurse (expr : Expr.z_t) (state : Action.avail_actions) :
      Action.avail_actions =
    match expr with
    | EUnOp_L (_, child)
    | EBinOp_L (child, _, _)
    | EBinOp_R (_, _, child)
    | ELet_L (_, child, _) (* variable not in self-scope in definition*)
    | EIf_L (child, _, _)
    | EIf_C (_, child, _)
    | EIf_R (_, _, child)
    | EFix_L (_, child)
    | EPair_L (child, _)
    | EPair_R (_, child) ->
        recurse child state
    (*functions: update  *)
    | EFun_L (varname, child) | ELet_R (varname, _, child) ->
        recurse child
          {
            move_parent = state.move_parent;
            max_child = state.max_child;
            in_scope = update_var_arr varname state.in_scope;
          }
    (*Now finally we do our cursor logic *)
    | Cursor subtree ->
        {
          move_parent = state.move_parent;
          max_child =
            (match subtree with
            | EVar _ | EInt _ | EBool _ | EHole | ENil -> 0
            | EUnOp _ | EFun _ | EFix _ -> 1
            | EBinOp _ | ELet _ | EPair _ -> 2
            | EIf _ -> 3);
          in_scope = state.in_scope;
        }
  in
  recurse expr acts_init
 *)
(* Given an assignment number, load the unit test
   Input:
     - assignment : index of assignment
   Output:
     - the unit test for the assignment
*)
let load_tests (directory : string) (assignment : int) : testType list =
  let filename = directory ^ "/" ^ string_of_int assignment ^ "/test.ml" in
  let tests_cons = parse_file filename in
  let rec combine_tests (tests_cons : Expr.t) : testType list =
    match tests_cons with
    | EBinOp (EPair (EInt a, EInt b), OpCon, ENil) -> [ (a, b) ]
    | EBinOp (EPair (EInt a, EInt b), OpCon, tl) -> (a, b) :: combine_tests tl
    | _ -> raise (IOError "Test file in incorrect format.")
  in
  combine_tests tests_cons

(* Given an assignment number, load the code
   Input:
     - assignment : index of assignment
   Output:
     - the code for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int) :
    Expr.t =
  let filename =
    directory ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index
    ^ ".ml"
  in
  parse_file filename



(* Moved a few higher–level graph-processing functions over here to avoid module *)
(* Errors*)


let rec expr_to_list (e : Expr.z_t) : (Expr.graph * CursorInfo.t) =
  let add_node (nodes : Expr.node list) (tag : int) : Expr.node list * int =
    let new_nodes = nodes @ [ tag ] in
    (new_nodes, List.length nodes)
  in
  let add_edge (edges : Expr.edge list) (new_edge : Expr.edge) : Expr.edge list =
    new_edge :: edges
  in
  let add_var (var : Var.t) (index : int) (vars : Expr.varlist) : Expr.varlist =
      (var, index) :: vars
    in
  let find_var (target : string) (vars : Expr.varlist) : int =
    let indices =
      List.filter (fun (var, index) -> Var.equal target var) vars
    in
    match indices with
    | (_, index) :: tl -> index
    | [] -> raise (SyntaxError "Expression not closed")
  in
  let append_type_tree (nodes : Expr.node list) (edges : Expr.edge list)
      (ty_nodes : Expr.node list) (ty_edges : Expr.edge list) (root : int) : Expr.graph * int
      =
    let len = List.length nodes in
    let ty_edges =
      List.map (fun (x, y, z) -> (x + len, y + len, z)) ty_edges
    in
    ((nodes @ ty_nodes, edges @ ty_edges), root + len)
  in
  let rec to_list_aux (e : Expr.t) (nodes : Expr.node list) (edges : Expr.edge list)
      (vars : Expr.varlist) : Expr.graph * int * Expr.varlist =
    let add_subtree (e : Expr.t) (nodes : Expr.node list) (edges : Expr.edge list)
        (root : int) (num_child : int) : Expr.graph =
      let (nodes, edges), new_root, _ = to_list_aux e nodes edges vars in
      let edges = add_edge edges (root, new_root, num_child) in
      (nodes, edges)
    in
    let tag = Expr.node_to_tag e in
    let nodes, root = add_node nodes tag in
    match e with
    | EInt _ | EBool _ | EHole | ENil -> ((nodes, edges), root, vars)
    | EVar x ->
        let edges = add_edge edges (find_var x vars, root, -1) in
        ((nodes, edges), root, vars)
    | EUnOp (_, e) -> (add_subtree e nodes edges root 1, root, vars)
    | EBinOp (e1, _, e2) | EPair (e1, e2) ->
        let nodes, edges = add_subtree e1 nodes edges root 1 in
        (add_subtree e2 nodes edges root 2, root, vars)
    | EFun (x, ty, e) | EFix (x, ty, e) ->
        let nodes, new_root = add_node nodes (Expr.node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let vars = add_var x new_root vars in
        let (ty_nodes, ty_edges), new_root = Typ.to_list ty in
        let (nodes, edges), new_root =
          append_type_tree nodes edges ty_nodes ty_edges new_root
        in
        let edges = add_edge edges (root, new_root, 2) in
        (add_subtree e nodes edges root 3, root, vars)
    | ELet (x, edef, ebody) ->
        let nodes, new_root = add_node nodes (Expr.node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let nodes, edges = add_subtree edef nodes edges root 2 in
        let vars = add_var x new_root vars in
        (add_subtree ebody nodes edges root 3, root, vars)
    | EIf (econd, ethen, eelse) ->
        let nodes, edges = add_subtree econd nodes edges root 1 in
        let nodes, edges = add_subtree ethen nodes edges root 2 in
        (add_subtree eelse nodes edges root 3, root, vars)
  in
  let graph,_,_  = (to_list_aux (Expr.unzip_ast e) [] [] []) in
  (graph, get_cursor_info e) 

