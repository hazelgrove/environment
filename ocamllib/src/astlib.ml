(* Include functions to be used by CInterface *)

open Ast
open Astutil
open Type
open Var

exception SyntaxError of string
exception IOError of string
exception NotImplemented of unit
exception InvalidAction of int

type testType = int * int

(*
     Given an zippered AST, apply the action
     Input:
       - e : an AST with cursor
       - action : action applied to e
     Output:
       the modified AST
*)
let change_ast (tree : Expr.z_t) (action : Action.t) : Expr.z_t =
  (* Handles actions on type subtrees *)
  let rec act_on_type (type_tree : Typ.z_t) : Typ.z_t =
    
    let build_type (subtr : Typ.t) (shape : Action.shape) : Typ.z_t =
      (* actually creates the desired type at the cursor.*)
      (*Also handles child wrapping logics *)
      Cursor
        (match shape with
        | TypInt -> Typ.TInt
        | TypBool -> Typ.TBool
        | TypHole -> Typ.THole
        | TypArrow_L -> Typ.TArrow (subtr, Typ.THole)
        | TypArrow_R -> Typ.TArrow (Typ.THole, subtr)
        | TypList -> Typ.TList subtr
        | TypProd_L -> Typ.TProd (subtr, THole)
        | TypProd_R -> Typ.TProd (THole, subtr)
        | _ ->
            subtr (* all other shapes are for exprssions which are not valid*))
    in
    let rec construct (shape : Action.shape) (tree : Typ.z_t) : Typ.z_t =
      (*recurses to cursor in type tree and builds the appropriate tree*)
      (* at the cursor *)
      let construct_shape = construct shape in
      match tree with
      | Arrow_L (tl, tr) -> Arrow_L (construct_shape tl, tr)
      | Prod_L (tl, tr) -> Prod_L (construct_shape tl, tr)
      | Arrow_R (tl, tr) -> Arrow_R (tl, construct_shape tr)
      | Prod_R (tl, tr) -> Prod_R (tl, construct_shape tr)
      | List_L tl -> List_L (construct_shape tl)
      | Cursor subtr -> build_type subtr shape
    in
    let rec move_child (n : int) (tree : Typ.z_t) : Typ.z_t =
      (* handles move_child actions *)
      let move_n_child = move_child n in 
      (*helper variable to simplify downstream code*)
      match (n, type_tree) with
      | _, Arrow_L (tl, tr) -> Arrow_L (move_n_child tl, tr)
      | _, Prod_L (tl, tr) -> Prod_L (move_n_child tl, tr)
      | _, Arrow_R (tl, tr) -> Arrow_R (tl, move_n_child tr)
      | _, Prod_R (tl, tr) -> Prod_R (tl, move_n_child tr)
      | _, List_L tl -> List_L (move_n_child tl)
      (* construct appropriate child, else do nothing *)
      | 0, Typ.Cursor (TArrow (tl, tr)) -> Arrow_L (Typ.Cursor tl, tr)
      | 1, Typ.Cursor (TArrow (tl, tr)) -> Arrow_R (tl, Typ.Cursor tr)
      | 0, Typ.Cursor (TProd (tl, tr)) -> Prod_L (Typ.Cursor tl, tr)
      | 1, Typ.Cursor (TProd (tl, tr)) -> Prod_R (tl, Typ.Cursor tr)
      | 0, Typ.Cursor (TList tl) -> List_L (Typ.Cursor tl)
      | _ -> raise (InvalidAction (Action.action_to_tag action))
      (*other values are invalid *)
    in
    let move_parent (tree : Typ.z_t) : Typ.z_t =
      match tree with
      (*if child of current tree is the cursor move upwards*)
      | Arrow_L (Cursor subt, tr) -> Cursor (TArrow (subt, tr))
      | Arrow_R (tl, Cursor subt) -> Cursor (TArrow (tl, subt))
      | Prod_L (Cursor subt, tr) -> Cursor (TProd (subt, tr))
      | Prod_R (tl, Cursor subt) -> Cursor (TProd (tl, subt))
      (* else recurse *)
      | Arrow_L (tl, tr) -> Arrow_L (act_on_type tl, tr)
      | Prod_L (tl, tr) -> Prod_L (act_on_type tl, tr)
      | Arrow_R (tl, tr) -> Arrow_R (tl, act_on_type tr)
      | Prod_R (tl, tr) -> Prod_R (tl, act_on_type tr)
      | List_L tl -> List_L (act_on_type tl)
      (* otherwise we've reached the cursor: this can only happen if act_on_type is
         called directly on a type cursor, which shouldn't be possible when the action
         is move parent. *)
      | Cursor _ -> raise (InvalidAction (Action.action_to_tag action))
      (* for when cursor is reached (shouldnt happen)*)
    in
    (* actual switch statement that uses action to determine which subfuncc to call *)
    match action with
    | Construct shape -> construct shape type_tree
    | Move (Child n) -> move_child n type_tree
    | Move Parent -> move_parent type_tree
  in
  let build_expr (shape : Action.shape) (subtree : Expr.t) : Expr.t =
      (* builds the actual expression at the cursor for expression types *)
      (* handles wrapping logics appropriately *)
    match shape with
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
    | Fun varname -> EFun (varname, THole, subtree)
    | Fix varname -> EFix (varname, THole, subtree)
    | Pair_L -> EPair (subtree, EHole)
    | Pair_R -> EPair (EHole, subtree)
    (* throw invalid action if no actions match *)
    | _ -> raise (InvalidAction (Action.action_to_tag action))
    (* only other option is type 'shapes' which arent valid in this scope*)
  in
  let rec construct (shape : Action.shape) (tree : Expr.z_t) : Expr.z_t =
      (* recurses to cursor then constructs (and wraps) the appropriate node *)
    let construct_shape = construct shape in
    (* recurse to cursor *)
    match tree with
    | EUnOp_L (op, r_child) -> EUnOp_L (op, construct_shape r_child)
    | EBinOp_L (l_child, op, r_child) ->
        EBinOp_L (construct_shape l_child, op, r_child)
    | EBinOp_R (l_child, op, r_child) ->
        EBinOp_R (l_child, op, construct_shape r_child)
    | ELet_L (var, l_child, r_child) ->
        ELet_L (var, construct_shape l_child, r_child)
    | ELet_R (var, l_child, r_child) ->
        ELet_R (var, l_child, construct_shape r_child)
    | EIf_L (l, c, r) -> EIf_L (construct_shape l, c, r)
    | EIf_C (l, c, r) -> EIf_C (l, construct_shape c, r)
    | EIf_R (l, c, r) -> EIf_R (l, c, construct_shape r)
    | EFun_R (var, typ, child) -> EFun_R (var, typ, construct_shape child)
    | EFun_L (var, typ, child) -> EFun_L (var, act_on_type typ, child)
    | EFix_R (var, typ, child) -> EFix_R (var, typ, construct_shape child)
    | EFix_L (var, typ, child) -> EFix_L (var, act_on_type typ, child)
    | EPair_L (l_child, r_child) -> EPair_L (construct_shape l_child, r_child)
    | EPair_R (l_child, r_child) -> EPair_R (l_child, construct_shape r_child)
    (* at cursor, build the correct expression *)
    | Cursor subtree -> Cursor (build_expr shape subtree)
  in
  let shuffle_cursor (n_child : int) (subtree : Expr.t) : Expr.z_t =
    (* moves curser to appropriate child of expression in `subtree` *)
    match (n_child, subtree) with
    | 0, EUnOp (op, arg) -> EUnOp_L (op, Cursor arg)
    | 0, EBinOp (arg_l, op, arg_r) -> EBinOp_L (Cursor arg_l, op, arg_r)
    | 0, ELet (varn, arg_l, arg_r) -> ELet_L (varn, Cursor arg_l, arg_r)
    | 0, EIf (arg_l, arg_c, arg_r) -> EIf_L (Cursor arg_l, arg_c, arg_r)
    | 0, EFun (varname, typ, arg) -> EFun_L (varname, Typ.Cursor typ, arg)
    | 0, EFix (varname, typ, arg) -> EFix_L (varname, Typ.Cursor typ, arg)
    | 0, EPair (arg_l, arg_r) -> EPair_L (Cursor arg_l, arg_r)
    | 1, EBinOp (arg_l, op, arg_r) -> EBinOp_R (arg_l, op, Cursor arg_r)
    | 1, ELet (varn, arg_l, arg_r) -> ELet_R (varn, arg_l, Cursor arg_r)
    | 1, EIf (arg_l, arg_c, arg_r) -> EIf_C (arg_l, Cursor arg_c, arg_r)
    | 1, EPair (arg_l, arg_r) -> EPair_R (arg_l, Cursor arg_r)
    | 1, EFun (varname, typ, arg_l) -> EFun_R (varname, typ, Cursor arg_l)
    | 1, EFix (varname, typ, arg_l) -> EFix_R (varname, typ, Cursor arg_l)
    | 2, EIf (arg_l, arg_c, arg_r) -> EIf_R (arg_l, arg_c, Cursor arg_r)
    | _ -> raise (InvalidAction (Action.action_to_tag action))
    (*all invalid actions are noops*)
  in
  let rec move_child (n_child : int) (tree : Expr.z_t) : Expr.z_t =
    (* recurses to cursor then moves cursor to appropriate child*)
    let move_n_child = move_child n_child in
    match tree with
    | EUnOp_L (op, r_child) -> EUnOp_L (op, move_n_child r_child)
    | EBinOp_L (l_child, op, r_child) ->
        EBinOp_L (move_n_child l_child, op, r_child)
    | EBinOp_R (l_child, op, r_child) ->
        EBinOp_R (l_child, op, move_n_child r_child)
    | ELet_L (var, l_child, r_child) ->
        ELet_L (var, move_n_child l_child, r_child)
    | ELet_R (var, l_child, r_child) ->
        ELet_R (var, l_child, move_n_child r_child)
    | EIf_L (l, c, r) -> EIf_L (move_n_child l, c, r)
    | EIf_C (l, c, r) -> EIf_C (l, move_n_child c, r)
    | EIf_R (l, c, r) -> EIf_R (l, c, move_n_child r)
    | EFun_R (var, typ, child) -> EFun_R (var, typ, move_n_child child)
    | EFun_L (var, typ, child) -> EFun_L (var, act_on_type typ, child)
    | EFix_R (var, typ, child) -> EFix_R (var, typ, move_n_child child)
    | EFix_L (var, typ, child) -> EFix_L (var, act_on_type typ, child)
    | EPair_L (l_child, r_child) -> EPair_L (move_n_child l_child, r_child)
    | EPair_R (l_child, r_child) -> EPair_R (l_child, move_n_child r_child)
    (*Once cursor is reached, use dedicated func to move to appropriate subtree*)
    | Cursor subtree -> shuffle_cursor n_child subtree
  in
  let rec move_parent (tree : Expr.z_t) : Expr.z_t =
    (*Handles move-parent operations.
       for each node type, we have two options: 
        if the cursor is a direct child, move it upward 
        Otherwise, continue recursing into the tree *)
    match tree with
    | EUnOp_L (op, Cursor arg) -> Cursor (EUnOp (op, arg))
    | EUnOp_L (op, arg) -> EUnOp_L (op, move_parent arg)
    | EBinOp_L (Cursor arg, op, r_child) -> Cursor (EBinOp (arg, op, r_child))
    | EBinOp_L (l_child, op, r_child) ->
        EBinOp_L (move_parent l_child, op, r_child)
    | EBinOp_R (l_child, op, Cursor arg) -> Cursor (EBinOp (l_child, op, arg))
    | EBinOp_R (l_child, op, r_child) ->
        EBinOp_R (l_child, op, move_parent r_child)
    | EPair_L (Cursor l_child, r_child) -> Cursor (EPair (l_child, r_child))
    | EPair_L (l_child, r_child) -> EPair_L (move_parent l_child, r_child)
    | EPair_R (l_child, Cursor r_child) -> Cursor (EPair (l_child, r_child))
    | EPair_R (l_child, r_child) -> EPair_R (l_child, move_parent r_child)
    | ELet_L (var, Cursor arg, r_child) -> Cursor (ELet (var, arg, r_child))
    | ELet_L (var, l_child, r_child) ->
        ELet_L (var, move_parent l_child, r_child)
    | ELet_R (var, l_child, Cursor arg) -> Cursor (ELet (var, l_child, arg))
    | ELet_R (var, l_child, r_child) ->
        ELet_R (var, l_child, move_parent r_child)
    | EIf_L (Cursor arg, c, r) -> Cursor (EIf (arg, c, r))
    | EIf_L (l, c, r) -> EIf_L (move_parent l, c, r)
    | EIf_C (l, Cursor arg, r) -> Cursor (EIf (l, arg, r))
    | EIf_C (l, c, r) -> EIf_C (l, move_parent c, r)
    | EIf_R (l, c, Cursor arg) -> Cursor (EIf (l, c, arg))
    | EIf_R (l, c, r) -> EIf_R (l, c, move_parent r)
    | EFun_L (var, Typ.Cursor typ, arg) -> Cursor (EFun (var, typ, arg))
    | EFun_L (var, typ, arg) -> EFun_L (var, act_on_type typ, arg)
    | EFix_L (var, Typ.Cursor typ, arg) -> Cursor (EFix (var, typ, arg))
    | EFix_L (var, typ, arg) -> EFix_L (var, act_on_type typ, arg)
    | EFun_R (var, typ, Cursor arg) -> Cursor (EFun (var, typ, arg))
    | EFun_R (var, typ, child) -> EFun_R (var, typ, move_parent child)
    | EFix_R (var, typ, Cursor arg) -> Cursor (EFix (var, typ, arg))
    | EFix_R (var, typ, child) -> EFix_R (var, typ, move_parent child)
    | _ -> raise (InvalidAction (Action.action_to_tag action))
  in
  let act_on (tree : Expr.z_t) : Expr.z_t =
    match action with
    | Construct shape -> construct shape tree
    | Move (Child n) -> move_child n tree
    | Move Parent -> move_parent tree
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
        | EFun _ | EFix _ -> (
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

let%test_module "Test run_unit_tests" =
  (module struct
    (* All correct *)
    let%test _ =
      run_unit_tests [ (1, 2); (-2, -1); (0, 1) ] (parse "let f n = n + 1")
      = true

    (* Partially correct *)
    let%test _ =
      run_unit_tests [ (1, 2); (-2, 0); (0, 1) ] (parse "let f n = n + 1")
      = false

    (* Incorrect *)
    let%test _ =
      run_unit_tests [ (1, 3); (-2, 0); (0, 2) ] (parse "let f n = n + 1")
      = false

    (* Error in code *)
    let%test _ =
      run_unit_tests [ (1, 2); (-2, -1); (0, 1) ] (parse "let f n = n + true")
      = false

    (* Error in format *)
    let%test _ =
      run_unit_tests
        [ (1, 2); (-2, -1); (0, 1) ]
        (parse "let f n = n + true in f 1")
      = false
  end)

(* syn and ana*)
let rec synthesis (context : Assumptions.t) (e : Expr.t) : Typ.t option =
  (*given an expression and its type-context, infer its type by looking 
     at its children, if possible *)
  match e with  (*match epxression based on type (and operation for unops and binops)*)
  | EVar x -> Assumptions.lookup context x
  | EInt _ -> Some TInt
  | EBool _ -> Some TBool
  | EUnOp (OpNeg, arg) -> (* negation: if child is int, expr has same type*)
      if analysis context arg Typ.TInt then Some TInt else None
  | EBinOp (argl, (OpPlus | OpMinus | OpTimes | OpDiv), argr) -> 
    (*arithmetic operations: if we see an int, return an int *)
      if analysis context argl TInt && analysis context argr TInt
      then Some TInt
      else None
  | EBinOp (argl, (OpGt | OpGe | OpLt | OpLe), argr) ->(*comparasons: int-> bool*)
      if analysis context argl TInt && analysis context argr TInt
      then Some TBool
      else None
  | EBinOp (argl, (OpEq | OpNe), argr) ->
      (* equal is a special case*)
      if (analysis context argl TInt && analysis context argr TInt)
         || (analysis context argl TBool && analysis context argr TBool)
      then Some TBool
      else None
  | EBinOp (arrow, OpAp, arg) -> (
      match synthesis context arrow with
      | Some (TArrow (in_t, out_t)) ->
          if analysis context arg in_t then Some out_t else None
      | _ -> None)
  | EBinOp (hd, OpCons, tl) -> (
      match synthesis context tl with
      | Some (TList list_t) ->
          if analysis context hd list_t then Some (TList list_t) else None
      | _ -> None)
  | EPair (l_pair, r_pair) -> (
      match (synthesis context l_pair, synthesis context r_pair) with
      | Some l_t, Some r_t -> Some (TProd (l_t, r_t))
      | _ -> None)
  | EIf (argl, argc, argr) ->
      if analysis context argl TBool
      then
        match synthesis context argc with
        | Some out_t ->
            if analysis context argc out_t && analysis context argr out_t
            then Some out_t
            else None
        | _ -> None
      else None
  | ELet (varn, dec, body) -> (
      match synthesis context dec with
      | Some var_t -> synthesis (Assumptions.extend context (varn, var_t)) body
      | _ -> None)
  (* | ELet (varn, Some vart, dec, body) -> if analysis context dec vart
     then synthesis (Assumptions.extend context (varn,var_t)) body
     else None *)
  | EFun (varn, vart, body) -> (
      match synthesis (Assumptions.extend context (varn, vart)) body with
      | Some outtype -> Some (TArrow (vart, outtype))
      | _ -> None)
  (* | EFun (varn, vart, Some outtype, body) ->
     if analysis (Assumptions.extend context (varn,vart)) body outtype
       then Some Arrow (vart,outtype) else None *)
  | EFix (varn, vart, body) ->
      if analysis (Assumptions.extend context (varn, vart)) body vart
      then Some vart
      else None
  | EHole -> None
  | ENil -> Some (TList THole)

and analysis (context : Assumptions.t) (e : Expr.t) (targ : Typ.t) : bool =
  (* given an epxression and an expected type, 
      return a bool representing whether that's correct*)
  match e with
  | EFun (varn, vart, expr) -> (
      match synthesis (Assumptions.extend context (varn, vart)) expr with
      | Some etyp -> Typ.consistent etyp targ
      | None -> false)
  | EFix (varn, vart, arg) -> Typ.consistent vart targ && analysis context arg targ
  | EPair (lpair, rpair) -> (
      match targ with
      | TProd (l_t, r_t) ->
          analysis context lpair l_t && analysis context rpair r_t
      | _ -> false)
  | EIf (argl, argc, argr) ->
     (* for if statements, first arg is expected to be a bool,
        and second and third are expected to match *)
      analysis context argl TBool
      && analysis context argc targ && analysis context argr targ
  | ELet (varn, dec, body) -> (   
    (* for variable declarations, add variable type to context*)
      let var_t = synthesis context dec in
      match var_t with
      | Some vart ->
          analysis (Assumptions.extend context (varn, vart)) body targ
      | None -> false)
  | _ -> (
      match synthesis context e with
      | None -> false
      | Some expt -> Typ.consistent expt targ) (* this handles all the other cases*)

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
    | EBinOp (EPair (EInt a, EInt b), OpCons, ENil) -> [ (a, b) ]
    | EBinOp (EPair (EInt a, EInt b), OpCons, tl) -> (a, b) :: combine_tests tl
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

let get_cursor_info (e : SyntaxTree.z_t) : CursorInfo.t =
  let rec recurse (node : SyntaxTree.z_t) (parent : SyntaxTree.t option)
      (def_cont : (Var.t * int) list) (typ_cont : Assumptions.t)
      (pred_type : Typ.t option) (ind : int) :
      CursorInfo.t
      =
    let current : SyntaxTree.t =
      match node with
      | ZENode zparent -> ENode (Expr.unzip_ast zparent)
      | ZTNode zparent -> TNode (Typ.unzip zparent)
    in
    match node with
    | ZENode (Cursor cursed) ->
        {
          current_term = ENode cursed;
          parent_term = parent;
          ctx = def_cont;
          typ_ctx = typ_cont;
          expected_ty = pred_type;
          actual_ty = synthesis typ_cont cursed;
        }
    | ZENode (EUnOp_L (OpNeg, argl)) ->
        let exp_typ =
          match pred_type with Some TBool | Some TInt -> pred_type | _ -> None
        in
        recurse (ZENode argl) (Some current) def_cont typ_cont exp_typ ind
    | ZENode
        (EBinOp_L
          ( argl,
            (OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe),
            argr )) ->
        recurse (ZENode argl) (Some current) def_cont typ_cont (Some TInt) ind
    | ZENode
        (EBinOp_R
          ( argl,
            (OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe),
            argr )) ->
        recurse (ZENode argr) (Some current) def_cont typ_cont (Some TInt)
          (ind + SyntaxTree.size (ENode argl) + 1)
    | ZENode (EBinOp_L (argl, (OpEq | OpNe), argr)) ->
        let exp_typ =
          match pred_type with Some TBool | Some TInt -> pred_type | _ -> None
        in
        recurse (ZENode argl) (Some current) def_cont typ_cont exp_typ ind
    | ZENode (EBinOp_R (argl, (OpEq | OpNe), argr)) ->
        let exp_typ =
          match pred_type with Some TBool | Some TInt -> pred_type | _ -> None
        in
        recurse (ZENode argr) (Some current) def_cont typ_cont exp_typ ind
    | ZENode (EBinOp_L (argl, OpCons, arr)) ->
        let exp_typ =
          match (pred_type, synthesis typ_cont arr) with
          | Some (TList ltype), _ -> Some ltype
          | _, Some (TList ltype) -> Some ltype
          | _ -> None
        in
        recurse (ZENode argl) (Some current) def_cont typ_cont exp_typ ind
    | ZENode (EBinOp_R (argl, OpCons, arr)) ->
        let exp_typ =
          match (pred_type, synthesis typ_cont argl) with
          | Some (TList ltype), _ -> Some ltype
          | _, Some (TList ltype) -> Some ltype
          | _ -> None
        in
        recurse (ZENode arr) (Some current) def_cont typ_cont exp_typ
          (ind + SyntaxTree.size (ENode argl) + 1)
    | ZENode (EBinOp_L (argl, OpAp, argr)) -> (
        match (pred_type, synthesis typ_cont argr) with
        | Some out_type, Some inp_type ->
            recurse (ZENode argl) (Some current) def_cont typ_cont
              (Some (TArrow (inp_type, out_type)))
              ind
        | _ -> recurse (ZENode argl) (Some current) def_cont typ_cont None ind)
    | ZENode (EBinOp_R (argl, OpAp, argr)) -> (
        match synthesis typ_cont argl with
        | Some (TArrow (in_t, _)) ->
            recurse (ZENode argr) (Some current) def_cont typ_cont (Some in_t)
              (ind + SyntaxTree.size (ENode argl) + 1)
        | _ ->
            recurse (ZENode argr) (Some current) def_cont typ_cont None
              (ind + SyntaxTree.size (ENode argl) + 1))
    | ZENode (ELet_L (_, argl, argr)) ->
        recurse (ZENode argl) (Some current) def_cont typ_cont None ind
    | ZENode (ELet_R (varn, argl, argr)) -> (
        match synthesis typ_cont argl with
        | Some lettype ->
            recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
              ((varn, lettype) :: typ_cont)
              None
              (ind + SyntaxTree.size (ENode argl) + 1)
        | None ->
            recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
              ((varn, THole) :: typ_cont)
              None
              (ind + SyntaxTree.size (ENode argl) + 1))
    | ZENode (EIf_L (argl, argc, argr)) ->
        recurse (ZENode argl) (Some current) def_cont typ_cont (Some TBool) ind
    | ZENode (EIf_C (argl, argc, argr)) ->
        recurse (ZENode argc) (Some current) def_cont typ_cont
          (synthesis typ_cont argr)
          (ind + SyntaxTree.size (ENode argl) + 1)
    | ZENode (EIf_R (argl, argc, argr)) ->
        recurse (ZENode argr) (Some current) def_cont typ_cont
          (synthesis typ_cont argc)
          (ind + SyntaxTree.size (ENode argl) + SyntaxTree.size (ENode argc) + 1)
    | ZENode (EFun_L (_, typ, argr)) ->
        recurse (ZTNode typ) (Some current) def_cont typ_cont None ind
    | ZENode (EFun_R (varn, typ, argr)) ->
        recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
          ((varn, typ) :: typ_cont) None
          (ind + SyntaxTree.size (TNode typ) + 1)
    | ZENode (EFix_L (_, typ, argr)) ->
        recurse (ZTNode typ) (Some current) def_cont typ_cont None ind
    | ZENode (EFix_R (varn, typ, argr)) ->
        recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
          ((varn, typ) :: typ_cont) None
          (ind + SyntaxTree.size (TNode typ) + 1)
    | ZENode (EPair_L (argl, argr)) -> (
        match pred_type with
        | Some (TProd (ltyp, _)) ->
            recurse (ZENode argl) (Some current) def_cont typ_cont (Some ltyp)
              ind
        | _ -> recurse (ZENode argl) (Some current) def_cont typ_cont None ind)
    | ZENode (EPair_R (argl, argr)) -> (
        match pred_type with
        | Some (TProd (_, rtype)) ->
            recurse (ZENode argr) (Some current) def_cont typ_cont (Some rtype)
              (ind + SyntaxTree.size (ENode argl) + 1)
        | _ -> recurse (ZENode argr) (Some current) def_cont typ_cont None ind)
    | ZTNode (Cursor typ) ->
        {
          current_term = TNode typ;
          parent_term = parent;
          ctx = def_cont;
          typ_ctx = typ_cont;
          expected_ty = None;
          actual_ty = None;
        }
    | ZTNode (Arrow_L (in_typ, out_typ)) ->
        recurse (ZTNode in_typ) (Some current) def_cont typ_cont None ind
    | ZTNode (Arrow_R (in_typ, out_typ)) ->
        recurse (ZTNode out_typ) (Some current) def_cont typ_cont None
          (ind + SyntaxTree.size (TNode in_typ) + 1)
    | ZTNode (Prod_L (l_typ, r_typ)) ->
        recurse (ZTNode l_typ) (Some current) def_cont typ_cont None ind
    | ZTNode (Prod_R (l_typ, r_typ)) ->
        recurse (ZTNode r_typ) (Some current) def_cont typ_cont None
          (ind + SyntaxTree.size (TNode l_typ) + 1)
    | ZTNode (List_L l_typ) ->
        recurse (ZTNode l_typ) (Some current) def_cont typ_cont None ind
  in
  recurse e None [] [] None 0

let cursor_info_to_actions (info : CursorInfo.t) : Action.t list =
  let handle_root (ci : CursorInfo.t) (currlist : Action.t list) : Action.t list
      =
    match ci.parent_term with
    | Some _ -> Move Parent :: currlist
    | None -> currlist
  in
  let handle_children (currlist : Action.t list) : Action.t list =
    match info.current_term with
    | ENode (EIf _) ->
        Move (Child 0) :: Move (Child 1) :: Move (Child 2) :: currlist
    | ENode (EBinOp _ | EFun _ | ELet _ | EFix _ | EPair _)
    | TNode (TArrow _ | TProd _) ->
        Move (Child 0) :: Move (Child 1) :: currlist
    | ENode (EUnOp _) | TNode (TList _) -> Move (Child 0) :: currlist
    | ENode (EVar _ | EInt _ | EBool _ | EHole | ENil)
    | TNode (TInt | TBool | THole) ->
        currlist
  in
  let handle_constr_typ (currlist : Action.t list) : Action.t list =
    Construct TypInt :: Construct TypBool :: Construct TypArrow_L
    :: Construct TypArrow_R :: Construct TypList :: Construct TypHole
    :: Construct TypProd_L :: Construct TypProd_R :: currlist
  in
  let handle_constr_arithmetic_binops (currlist : Action.t list) : Action.t list
      =
    match (info.expected_ty, info.actual_ty) with
    | None, None (* always allow none none's *) | Some TInt, Some TInt ->
        Construct (BinOp_L OpPlus) :: Construct (BinOp_L OpMinus)
        :: Construct (BinOp_L OpTimes) :: Construct (BinOp_L OpDiv)
        :: Construct (BinOp_R OpPlus) :: Construct (BinOp_R OpMinus)
        :: Construct (BinOp_R OpTimes) :: Construct (BinOp_R OpDiv) :: currlist
    | _ -> currlist
  in
  let handle_comp_binops (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | None, None (* always allow none none's *) | Some TBool, Some TInt ->
        Construct (BinOp_L OpLt) :: Construct (BinOp_R OpLt)
        :: Construct (BinOp_L OpLe) :: Construct (BinOp_R OpLe)
        :: Construct (BinOp_L OpGt) :: Construct (BinOp_R OpGt)
        :: Construct (BinOp_L OpGe) :: Construct (BinOp_R OpGe) :: currlist
    | _ -> currlist
  in
  let handle_eq_binops (currlist : Action.t list) : Action.t list =
    match info.expected_ty with
    | None | Some TBool ->
        Construct (BinOp_L OpEq) :: Construct (BinOp_L OpNe)
        :: Construct (BinOp_R OpEq) :: Construct (BinOp_R OpNe) :: currlist
    | _ -> currlist
  in
  let handle_con_binop (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TList a), Some (TList b) ->
        if Typ.equal b THole
        then Construct (BinOp_R OpCons) :: currlist
        else if Typ.equal a b
        then Construct (BinOp_R OpCons) :: Construct (BinOp_L OpCons) :: currlist
        else currlist
    | Some (TList a), Some b ->
        if Typ.equal a b
        then Construct (BinOp_L OpCons) :: currlist
        else currlist
    | None, Some (TList _) ->
        Construct (BinOp_R OpCons) :: Construct (BinOp_L OpCons) :: currlist
    | None, Some _ -> Construct (BinOp_L OpCons) :: currlist
    | _ -> currlist
  in
  let handle_ap_binop (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some c, Some (TArrow (_, b)) ->
        if Typ.equal c b then Construct (BinOp_L OpAp) :: currlist else currlist
    | Some c, Some b -> Construct (BinOp_R OpAp) :: currlist
    | None, Some (TArrow _) ->
        Construct (BinOp_R OpAp) :: Construct (BinOp_L OpAp) :: currlist
    | None, Some _ -> Construct (BinOp_R OpAp) :: currlist
    | _ -> currlist
  in
  let handle_basic_types (currlist : Action.t list) : Action.t list =
    match info.expected_ty with
    | Some TInt -> Construct (Int 0) :: currlist
    | Some TBool -> Construct (Bool false) :: currlist
    | Some THole -> Construct Hole :: currlist
    | Some (TList _) -> Construct Nil :: currlist
    | None ->
        Construct (Int 0) :: Construct (Bool false) :: Construct Hole
        :: Construct Nil :: currlist
    | _ -> currlist
  in
  let handle_var (currlist : Action.t list) : Action.t list =
    let valid_vars =
      match info.expected_ty with
      | Some targ_type ->
          List.filter (fun (var, typ) -> Typ.equal typ targ_type) info.typ_ctx
      | None -> info.typ_ctx
    in
    List.map (fun (varn, typ) -> Action.Construct (Var varn)) valid_vars
    @ currlist
  in
  let handle_let (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    (* afik we can make let's anywhere*)
    | Some a, Some b ->
        if Typ.equal a b
        then Construct (Let_L "") :: Construct (Let_R "") :: currlist
        else Construct (Let_L "") :: currlist
    | _ -> Construct (Let_L "") :: Construct (Let_R "") :: currlist
  in
  let handle_if (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | _, Some TBool ->
        Construct If_L :: Construct If_C :: Construct If_R :: currlist
    | _ -> Construct If_C :: Construct If_R :: currlist
  in
  let handle_fun (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TArrow (a, b)), Some c ->
        if Typ.equal b c then Construct (Fun "") :: currlist else currlist
    | None, _ -> Construct (Fun "") :: currlist
    | _ -> currlist
  in
  let handle_fix (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TArrow (a, b)), Some c ->
        if Typ.equal b c then Construct (Fix "") :: currlist else currlist
    | None, _ -> Construct (Fix "") :: currlist
    | _ -> currlist
  in
  let handle_pair (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TProd (a, b)), Some c ->
        let left_half =
          if Typ.equal a c
          then Action.Construct Pair_L :: currlist
          else currlist
        in
        if Typ.equal b c then Construct Pair_R :: left_half else left_half
    | None, _ -> Construct Pair_L :: Construct Pair_R :: currlist
    | _ -> currlist
  in
  let currlist = handle_root info [] in
  let currlist = handle_children currlist in
  match info.current_term with
  | TNode _ -> handle_constr_typ currlist
  | ENode _ ->
      let currlist = handle_constr_arithmetic_binops currlist in
      let currlist = handle_comp_binops currlist in
      let currlist = handle_eq_binops currlist in
      let currlist = handle_con_binop currlist in
      let currlist = handle_ap_binop currlist in
      let currlist = handle_basic_types currlist in
      let currlist = handle_var currlist in
      let currlist = handle_let currlist in
      let currlist = handle_if currlist in
      let currlist = handle_fun currlist in
      let currlist = handle_fix currlist in
      handle_pair currlist

let expr_to_list (e : Expr.z_t) : Expr.graph * CursorInfo.t =
  let add_node (nodes : Expr.node list) (tag : int) : Expr.node list * int =
    let new_nodes = nodes @ [ tag ] in
    (new_nodes, List.length nodes)
  in
  let add_edge (edges : Expr.edge list) (new_edge : Expr.edge) : Expr.edge list
      =
    new_edge :: edges
  in
  let add_var (var : Var.t) (index : int) (vars : Expr.varlist) : Expr.varlist =
    (var, index) :: vars
  in
  let find_var (target : string) (vars : Expr.varlist) : int =
    let indices = List.filter (fun (var, index) -> Var.equal target var) vars in
    match indices with
    | (_, index) :: tl -> index
    | [] -> raise (SyntaxError "Expression not closed")
  in
  let append_type_tree (nodes : Expr.node list) (edges : Expr.edge list)
      (ty_nodes : Expr.node list) (ty_edges : Expr.edge list) (root : int) :
      Expr.graph * int =
    let len = List.length nodes in
    let ty_edges = List.map (fun (x, y, z) -> (x + len, y + len, z)) ty_edges in
    ((nodes @ ty_nodes, edges @ ty_edges), root + len)
  in
  let rec to_list_aux (e : Expr.t) (nodes : Expr.node list)
      (edges : Expr.edge list) (vars : Expr.varlist) :
      Expr.graph * int * Expr.varlist =
    let add_subtree (e : Expr.t) (nodes : Expr.node list)
        (edges : Expr.edge list) (vars : Expr.varlist) (root : int)
        (num_child : int) : Expr.graph =
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
    | EUnOp (_, e) -> (add_subtree e nodes edges vars root 1, root, vars)
    | EBinOp (e1, _, e2) | EPair (e1, e2) ->
        let nodes, edges = add_subtree e1 nodes edges vars root 1 in
        (add_subtree e2 nodes edges vars root 2, root, vars)
    | EFun (x, ty, e) | EFix (x, ty, e) ->
        let nodes, new_root = add_node nodes (Expr.node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let vars = add_var x new_root vars in
        let (ty_nodes, ty_edges), new_root = Typ.to_list ty in
        let (nodes, edges), new_root =
          append_type_tree nodes edges ty_nodes ty_edges new_root
        in
        let edges = add_edge edges (root, new_root, 2) in
        (add_subtree e nodes edges vars root 3, root, vars)
    | ELet (x, edef, ebody) ->
        let nodes, new_root = add_node nodes (Expr.node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let nodes, edges = add_subtree edef nodes edges vars root 2 in
        let vars = add_var x new_root vars in
        (add_subtree ebody nodes edges vars root 3, root, vars)
    | EIf (econd, ethen, eelse) ->
        let nodes, edges = add_subtree econd nodes edges vars root 1 in
        let nodes, edges = add_subtree ethen nodes edges vars root 2 in
        (add_subtree eelse nodes edges vars root 3, root, vars)
  in
  let graph, _, _ = to_list_aux (Expr.unzip_ast e) [] [] [] in
  (graph, get_cursor_info (ZENode e))

let%test_module "Test expr_to_list" =
  (module struct
    let check_id e =
      let (nodes, edges), _ = expr_to_list (Cursor e) in
      let changed_tree = Expr.from_list nodes edges 0 in
      e = changed_tree

    let%test _ =
      check_id
        (EFun
           ( "x",
             THole,
             EBinOp (EBinOp (EInt 2, OpTimes, EVar "x"), OpPlus, EInt 1) ))

    let%test _ =
      check_id
        (ELet
           ( "x",
             EFix
               ( "x",
                 THole,
                 EFun
                   ( "y",
                     TInt,
                     EIf
                       ( EBinOp (EVar "y", OpLt, EInt 1),
                         EInt 1,
                         EBinOp
                           ( EVar "y",
                             OpTimes,
                             EBinOp
                               ( EVar "x",
                                 OpAp,
                                 EBinOp (EVar "y", OpMinus, EInt 1) ) ) ) ) ),
             EBinOp (EVar "x", OpAp, EInt 2) ))
  end)

let rec get_cursor_position (tree : SyntaxTree.z_t) =
  match tree with
  | ZENode e -> (
      match e with
      | Cursor _ -> 0
      | EUnOp_L (_, e) | EBinOp_L (e, _, _) | EIf_L (e, _, _) | EPair_L (e, _)
        ->
          1 + get_cursor_position (ZENode e)
      | EBinOp_R (e1, _, e2) | EIf_C (e1, e2, _) | EPair_R (e1, e2) ->
          1 + Expr.size e1 + get_cursor_position (ZENode e2)
      | ELet_L (_, e, _) -> 1 + 1 + get_cursor_position (ZENode e)
      | ELet_R (_, e1, e2) ->
          1 + 1 + Expr.size e1 + get_cursor_position (ZENode e2)
      | EIf_R (e1, e2, e3) ->
          1 + Expr.size e1 + Expr.size e2 + get_cursor_position (ZENode e3)
      | EFun_L (_, ty, _) | EFix_L (_, ty, _) ->
          1 + 1 + get_cursor_position (ZTNode ty)
      | EFun_R (_, ty, e) | EFix_R (_, ty, e) ->
          1 + 1 + Typ.size ty + get_cursor_position (ZENode e))
  | ZTNode ty -> (
      match ty with
      | Cursor _ -> 0
      | Arrow_L (ty, _) | Prod_L (ty, _) -> 1 + get_cursor_position (ZTNode ty)
      | Arrow_R (t1, t2) | Prod_R (t1, t2) ->
          1 + Typ.size t1 + get_cursor_position (ZTNode t2)
      | List_L ty -> 1 + get_cursor_position (ZTNode ty))

let%test_module "Test get_cursor_position" =
  (module struct
    let e = Expr.Cursor (parse "(2 + 3 / 10, true)")

    let%test _ = get_cursor_position (ZENode e) = 0

    let e = Expr.EBinOp_L (Expr.Cursor (parse "fun x -> x + 1"), OpAp, EInt 2)

    let%test _ = get_cursor_position (ZENode e) = 1

    let e =
      Expr.ELet_R
        ( "x",
          parse "2 + 3",
          EBinOp_R (parse "10", OpTimes, Expr.Cursor (EVar "x")) )

    let%test _ = get_cursor_position (ZENode e) = 7

    let e =
      Expr.EFun_R
        ( "x",
          TArrow (TInt, TInt),
          EBinOp_R (parse "x", OpTimes, Expr.Cursor (EInt 2)) )

    let%test _ = get_cursor_position (ZENode e) = 7

    let e =
      Expr.EFun_L
        ("x", Arrow_L (Cursor TInt, TInt), EBinOp (parse "x", OpTimes, EInt 2))

    let%test _ = get_cursor_position (ZENode e) = 3
  end)
