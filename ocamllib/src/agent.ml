(* Functions for agent to apply actions to AST *)
exception InvalidAction of int

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
  let rec act_on_type (type_tree : Type.z_t) : Type.z_t =
    let build_type (subtr : Type.t) (shape : Action.shape) : Type.z_t =
      (* actually creates the desired type at the cursor.*)
      (*Also handles child wrapping logics *)
      Cursor
        (match shape with
        | TypInt -> Type.TInt
        | TypBool -> Type.TBool
        | TypHole -> Type.THole
        | TypArrow_L -> Type.TArrow (subtr, Type.THole)
        | TypArrow_R -> Type.TArrow (Type.THole, subtr)
        | TypList -> Type.TList subtr
        | TypProd_L -> Type.TProd (subtr, THole)
        | TypProd_R -> Type.TProd (THole, subtr)
        | _ ->
            subtr (* all other shapes are for exprssions which are not valid*))
    in
    let rec construct (shape : Action.shape) (tree : Type.z_t) : Type.z_t =
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
    let rec move_child (n : int) (tree : Type.z_t) : Type.z_t =
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
      | 0, Type.Cursor (TArrow (tl, tr)) -> Arrow_L (Type.Cursor tl, tr)
      | 1, Type.Cursor (TArrow (tl, tr)) -> Arrow_R (tl, Type.Cursor tr)
      | 0, Type.Cursor (TProd (tl, tr)) -> Prod_L (Type.Cursor tl, tr)
      | 1, Type.Cursor (TProd (tl, tr)) -> Prod_R (tl, Type.Cursor tr)
      | 0, Type.Cursor (TList tl) -> List_L (Type.Cursor tl)
      | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
      (*other values are invalid *)
    in
    let move_parent (tree : Type.z_t) : Type.z_t =
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
      | Cursor _ -> raise (InvalidAction (ActionConv.action_to_tag action))
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
    | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
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
    | 0, EFun (varname, typ, arg) -> EFun_L (varname, Type.Cursor typ, arg)
    | 0, EFix (varname, typ, arg) -> EFix_L (varname, Type.Cursor typ, arg)
    | 0, EPair (arg_l, arg_r) -> EPair_L (Cursor arg_l, arg_r)
    | 1, EBinOp (arg_l, op, arg_r) -> EBinOp_R (arg_l, op, Cursor arg_r)
    | 1, ELet (varn, arg_l, arg_r) -> ELet_R (varn, arg_l, Cursor arg_r)
    | 1, EIf (arg_l, arg_c, arg_r) -> EIf_C (arg_l, Cursor arg_c, arg_r)
    | 1, EPair (arg_l, arg_r) -> EPair_R (arg_l, Cursor arg_r)
    | 1, EFun (varname, typ, arg_l) -> EFun_R (varname, typ, Cursor arg_l)
    | 1, EFix (varname, typ, arg_l) -> EFix_R (varname, typ, Cursor arg_l)
    | 2, EIf (arg_l, arg_c, arg_r) -> EIf_R (arg_l, arg_c, Cursor arg_r)
    | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
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
    | EFun_L (var, Type.Cursor typ, arg) -> Cursor (EFun (var, typ, arg))
    | EFun_L (var, typ, arg) -> EFun_L (var, act_on_type typ, arg)
    | EFix_L (var, Type.Cursor typ, arg) -> Cursor (EFix (var, typ, arg))
    | EFix_L (var, typ, arg) -> EFix_L (var, act_on_type typ, arg)
    | EFun_R (var, typ, Cursor arg) -> Cursor (EFun (var, typ, arg))
    | EFun_R (var, typ, child) -> EFun_R (var, typ, move_parent child)
    | EFix_R (var, typ, Cursor arg) -> Cursor (EFix (var, typ, arg))
    | EFix_R (var, typ, child) -> EFix_R (var, typ, move_parent child)
    | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
  in
  let act_on (tree : Expr.z_t) : Expr.z_t =
    match action with
    | Construct shape -> construct shape tree
    | Move (Child n) -> move_child n tree
    | Move Parent -> move_parent tree
  in
  act_on tree
