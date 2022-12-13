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
let perform_action (tree : Expr.z_t) (action : Action.t) : Expr.z_t =
  (* Handles actions on type subtrees *)
  let rec act_on_type (type_tree : Type.z_t) : Type.z_t =
    let build_type (subtr : Type.t) (shape : Action.shape) : Type.z_node =
      (* actually creates the desired type at the cursor.*)
      (*Also handles child wrapping logics *)
      let node =
        match shape with
        | TypInt -> Type.TInt
        | TypBool -> Type.TBool
        | TypHole -> Type.THole
        | TypUnit -> Type.TUnit
        | TypArrow_L -> Type.TArrow (subtr, Type.make_node Type.THole)
        | TypArrow_R -> Type.TArrow (Type.make_node Type.THole, subtr)
        | TypList -> Type.TList subtr
        | TypProd_L -> Type.TProd (subtr, Type.make_node THole)
        | TypProd_R -> Type.TProd (Type.make_node THole, subtr)
        | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
      in
      Cursor node
    in
    let rec construct (shape : Action.shape) (tree : Type.z_t) : Type.z_t =
      (*recurses to cursor in type tree and builds the appropriate tree*)
      (* at the cursor *)
      let construct_shape = construct shape in
      let node : Type.z_node =
        match tree.node with
        | Arrow_L (tl, tr) -> Arrow_L (construct_shape tl, tr)
        | Prod_L (tl, tr) -> Prod_L (construct_shape tl, tr)
        | Arrow_R (tl, tr) -> Arrow_R (tl, construct_shape tr)
        | Prod_R (tl, tr) -> Prod_R (tl, construct_shape tr)
        | List_L tl -> List_L (construct_shape tl)
        | Cursor subtr -> build_type (Type.unzip tree) shape
      in
      {
        tree with
        id = (match tree.node with Cursor _ -> Id.generate () | _ -> tree.id);
        node;
      }
    in
    let rec move_child (n : int) (tree : Type.z_t) : Type.z_t =
      (* handles move_child actions *)
      let move_n_child = move_child n in
      (*helper variable to simplify downstream code*)
      let node : Type.z_node =
        match (n, tree.node) with
        | _, Arrow_L (tl, tr) -> Arrow_L (move_n_child tl, tr)
        | _, Prod_L (tl, tr) -> Prod_L (move_n_child tl, tr)
        | _, Arrow_R (tl, tr) -> Arrow_R (tl, move_n_child tr)
        | _, Prod_R (tl, tr) -> Prod_R (tl, move_n_child tr)
        | _, List_L tl -> List_L (move_n_child tl)
        (* construct appropriate child, else do nothing *)
        | 0, Type.Cursor (TArrow (tl, tr)) -> Arrow_L (Type.select_root tl, tr)
        | 1, Type.Cursor (TArrow (tl, tr)) -> Arrow_R (tl, Type.select_root tr)
        | 0, Type.Cursor (TProd (tl, tr)) -> Prod_L (Type.select_root tl, tr)
        | 1, Type.Cursor (TProd (tl, tr)) -> Prod_R (tl, Type.select_root tr)
        | 0, Type.Cursor (TList tl) -> List_L (Type.select_root tl)
        | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        (*other values are invalid *)
      in
      { tree with node }
    in
    let move_parent (tree : Type.z_t) : Type.z_t =
      let node =
        match tree.node with
        (*if child of current tree is the cursor move upwards*)
        | Arrow_L (({ node = Cursor subt; _ } as subtr), tr) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Type.Cursor (TArrow ({ (Type.unzip subtr) with node = subt }, tr))
        | Arrow_R (tl, ({ node = Cursor subt; _ } as subtr)) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Type.Cursor (TArrow (tl, { (Type.unzip subtr) with node = subt }))
        | Prod_L (({ node = Cursor subt; _ } as subtr), tr) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Type.Cursor (TProd ({ (Type.unzip subtr) with node = subt }, tr))
        | Prod_R (tl, ({ node = Cursor subt; _ } as subtr)) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Type.Cursor (TProd (tl, { (Type.unzip subtr) with node = subt }))
        (* else recurse *)
        | Arrow_L (tl, tr) -> Arrow_L (act_on_type tl, tr)
        | Prod_L (tl, tr) -> Prod_L (act_on_type tl, tr)
        | Arrow_R (tl, tr) -> Arrow_R (tl, act_on_type tr)
        | Prod_R (tl, tr) -> Prod_R (tl, act_on_type tr)
        | List_L tl -> List_L (act_on_type tl)
        (* otherwise we've reached the cursor: this can only happen if act_on_type is
            called directly on a type cursor, which shouldn't be possible when the action
            is move parent. *)
        | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        (* for when cursor is reached (shouldnt happen)*)
      in
      { tree with node }
    in
    (* actual switch statement that uses action to determine which subfuncc to call *)
    match action with
    | Construct shape -> construct shape type_tree
    | Move (Child n) -> move_child n type_tree
    | Move Parent -> move_parent type_tree
    | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
  in
  let rec act_on_pattern (p : Pattern.z_t) : Pattern.z_t =
    let build_pattern (subtr : Pattern.t) (shape : Action.shape) :
        Pattern.z_node =
      (* actually creates the desired type at the cursor.*)
      (*Also handles child wrapping logics *)
      let rec free_vars (p : Pattern.t) : unit =
        match p.node with
        | PConst _ | PWild -> ()
        | PVar x -> Var.free_var x
        | PCons (p1, p2) ->
            free_vars p1;
            free_vars p2
      in
      let node =
        match shape with
        | PatConst c ->
            free_vars subtr;
            Pattern.PConst c
        | PatVar ->
            free_vars subtr;
            Pattern.PVar (Var.get_new_var ())
        | PatCons_L -> Pattern.PCons (subtr, Pattern.make_node Pattern.PWild)
        | PatCons_R -> Pattern.PCons (Pattern.make_node Pattern.PWild, subtr)
        | PatWild -> Pattern.PWild
        | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
      in
      Cursor node
    in
    let rec construct (shape : Action.shape) (tree : Pattern.z_t) : Pattern.z_t
        =
      (*recurses to cursor in type tree and builds the appropriate tree*)
      (* at the cursor *)
      let construct_shape = construct shape in
      let node : Pattern.z_node =
        match tree.node with
        | PCons_L (tl, tr) -> PCons_L (construct_shape tl, tr)
        | PCons_R (tl, tr) -> PCons_R (tl, construct_shape tr)
        | Cursor subtr -> build_pattern (Pattern.unzip tree) shape
      in
      {
        tree with
        id = (match tree.node with Cursor _ -> Id.generate () | _ -> tree.id);
        node;
      }
    in
    let rec move_child (n : int) (tree : Pattern.z_t) : Pattern.z_t =
      (* handles move_child actions *)
      let move_n_child = move_child n in
      (*helper variable to simplify downstream code*)
      let node : Pattern.z_node =
        match (n, tree.node) with
        | _, PCons_L (tl, tr) -> PCons_L (move_n_child tl, tr)
        | _, PCons_R (tl, tr) -> PCons_R (tl, move_n_child tr)
        (* construct appropriate child, else do nothing *)
        | 0, Pattern.Cursor (PCons (tl, tr)) ->
            PCons_L (Pattern.select_root tl, tr)
        | 1, Pattern.Cursor (PCons (tl, tr)) ->
            PCons_R (tl, Pattern.select_root tr)
        | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        (*other values are invalid *)
      in
      { tree with node }
    in
    let move_parent (tree : Pattern.z_t) : Pattern.z_t =
      let node =
        match tree.node with
        (*if child of current tree is the cursor move upwards*)
        | PCons_L (({ node = Cursor subt; _ } as subtr), tr) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Pattern.Cursor
                (PCons ({ (Pattern.unzip subtr) with node = subt }, tr))
        | PCons_R (tl, ({ node = Cursor subt; _ } as subtr)) ->
            if tree.starter
            then raise (InvalidAction (ActionConv.action_to_tag action))
            else
              Pattern.Cursor
                (PCons (tl, { (Pattern.unzip subtr) with node = subt }))
        (* else recurse *)
        | PCons_L (tl, tr) -> PCons_L (act_on_pattern tl, tr)
        | PCons_R (tl, tr) -> PCons_R (tl, act_on_pattern tr)
        (* otherwise we've reached the cursor: this can only happen if act_on_type is
            called directly on a type cursor, which shouldn't be possible when the action
            is move parent. *)
        | Cursor _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        (* for when cursor is reached (shouldnt happen)*)
      in
      { tree with node }
    in
    let rec unwrap (tree : Pattern.z_t) (n : int) : Pattern.z_t =
      match tree.node with
      | Cursor e ->
          let subtree =
            match (n, e) with
            | 0, PCons (p, _) -> p
            | 1, PCons (_, p) -> p
            | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
          in
          Pattern.select_root subtree
      | _ ->
          let node : Pattern.z_node =
            match tree.node with
            | PCons_L (l_child, r_child) -> PCons_L (unwrap l_child n, r_child)
            | PCons_R (l_child, r_child) -> PCons_R (l_child, unwrap r_child n)
            | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
          in
          { tree with node }
    in
    (* actual switch statement that uses action to determine which subfuncc to call *)
    match action with
    | Construct shape -> construct shape p
    | Move (Child n) -> move_child n p
    | Move Parent -> move_parent p
    | Unwrap n -> unwrap p n
  in
  let build_expr (shape : Action.shape) (subtree : Expr.t) : Expr.z_node =
    let rec free_vars (e : Expr.t) : unit =
      let rec pattern_free_vars (p : Pattern.t) : unit =
        match p.node with
        | PConst _ | PWild -> ()
        | PVar x -> Var.free_var x
        | PCons (p1, p2) ->
            pattern_free_vars p1;
            pattern_free_vars p2
      in
      match e.node with
      | ELet (x, edef, ebody) ->
          Var.free_var x;
          free_vars edef;
          free_vars ebody
      | EFun (x, _, e) | EFix (x, _, e) ->
          Var.free_var x;
          free_vars e
      | EUnOp (_, e) -> free_vars e
      | EBinOp (e1, _, e2)
      | EPair (e1, e2)
      | EMap (e1, e2)
      | EFilter (e1, e2)
      | EListEq (e1, e2) ->
          free_vars e1;
          free_vars e2
      | EFold (e1, e2, e3) | EIf (e1, e2, e3) ->
          free_vars e1;
          free_vars e2;
          free_vars e3
      | EMatch (e, (p1, e1), (p2, e2)) ->
          free_vars e;
          pattern_free_vars p1;
          free_vars e1;
          pattern_free_vars p2;
          free_vars e2
      | EAssert e -> free_vars e
      | EConst _ | EHole | EVar _ -> ()
    in
    (* builds the actual expression at the cursor for expression types *)
    (* handles wrapping logics appropriately *)
    let node : Expr.node =
      match shape with
      | Var index ->
          free_vars subtree;
          let info = CursorInfo.get_cursor_info (Syntax.ZENode tree) in
          let varname, _ = List.nth info.vars_in_scope index in
          EVar varname
      | Arg index ->
          free_vars subtree;
          let info = CursorInfo.get_cursor_info (Syntax.ZENode tree) in
          let varname, _, _ = List.nth info.args_in_scope index in
          EVar varname
      | Hole ->
          free_vars subtree;
          EHole
      | Const c ->
          free_vars subtree;
          EConst c
      | UnOp op -> EUnOp (op, subtree)
      | BinOp_L op -> EBinOp (subtree, op, Expr.make_node EHole)
      | BinOp_R op -> EBinOp (Expr.make_node EHole, op, subtree)
      | Let_L -> ELet (Var.get_new_var (), subtree, Expr.make_node EHole)
      | Let_R -> ELet (Var.get_new_var (), Expr.make_node EHole, subtree)
      | If_L -> EIf (subtree, Expr.make_node EHole, Expr.make_node EHole)
      | If_C -> EIf (Expr.make_node EHole, subtree, Expr.make_node EHole)
      | If_R -> EIf (Expr.make_node EHole, Expr.make_node EHole, subtree)
      | Fold_L -> EFold (subtree, Expr.make_node EHole, Expr.make_node EHole)
      | Fold_C -> EFold (Expr.make_node EHole, subtree, Expr.make_node EHole)
      | Fold_R -> EFold (Expr.make_node EHole, Expr.make_node EHole, subtree)
      | Fun -> EFun (Var.get_new_var (), Type.make_node THole, subtree)
      | Fix -> EFix (Var.get_new_var (), Type.make_node THole, subtree)
      | Pair_L -> EPair (subtree, Expr.make_node EHole)
      | Pair_R -> EPair (Expr.make_node EHole, subtree)
      | Map_L -> EMap (subtree, Expr.make_node EHole)
      | Map_R -> EMap (Expr.make_node EHole, subtree)
      | Filter_L -> EFilter (subtree, Expr.make_node EHole)
      | Filter_R -> EFilter (Expr.make_node EHole, subtree)
      | Match_L ->
          EMatch
            ( subtree,
              (Pattern.make_node PWild, Expr.make_node EHole),
              (Pattern.make_node PWild, Expr.make_node EHole) )
      | Match_E1 ->
          EMatch
            ( Expr.make_node EHole,
              (Pattern.make_node PWild, subtree),
              (Pattern.make_node PWild, Expr.make_node EHole) )
      | Match_E2 ->
          EMatch
            ( Expr.make_node EHole,
              (Pattern.make_node PWild, Expr.make_node EHole),
              (Pattern.make_node PWild, subtree) )
      (* throw invalid action if no actions match *)
      | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
      (* only other option is type 'shapes' which arent valid in this scope*)
    in
    Cursor node
  in
  let rec construct (shape : Action.shape) (tree : Expr.z_t) : Expr.z_t =
    (* recurses to cursor then constructs (and wraps) the appropriate node *)
    let construct_shape = construct shape in
    let node : Expr.z_node =
      (* recurse to cursor *)
      match tree.node with
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
      | EFold_L (l, c, r) -> EFold_L (construct_shape l, c, r)
      | EFold_C (l, c, r) -> EFold_C (l, construct_shape c, r)
      | EFold_R (l, c, r) -> EFold_R (l, c, construct_shape r)
      | EFun_R (var, typ, child) -> EFun_R (var, typ, construct_shape child)
      | EFun_L (var, typ, child) -> EFun_L (var, act_on_type typ, child)
      | EFix_R (var, typ, child) -> EFix_R (var, typ, construct_shape child)
      | EFix_L (var, typ, child) -> EFix_L (var, act_on_type typ, child)
      | EPair_L (l_child, r_child) -> EPair_L (construct_shape l_child, r_child)
      | EPair_R (l_child, r_child) -> EPair_R (l_child, construct_shape r_child)
      | EMap_L (l_child, r_child) -> EMap_L (construct_shape l_child, r_child)
      | EMap_R (l_child, r_child) -> EMap_R (l_child, construct_shape r_child)
      | EFilter_L (l_child, r_child) ->
          EFilter_L (construct_shape l_child, r_child)
      | EFilter_R (l_child, r_child) ->
          EFilter_R (l_child, construct_shape r_child)
      | EListEq_L (l_child, r_child) ->
          EListEq_L (construct_shape l_child, r_child)
      | EListEq_R (l_child, r_child) ->
          EListEq_R (l_child, construct_shape r_child)
      | EMatch_L (l_child, (p1, e1), (p2, e2)) ->
          EMatch_L (construct_shape l_child, (p1, e1), (p2, e2))
      | EMatch_P1 (l_child, (p1, e1), (p2, e2)) ->
          EMatch_P1 (l_child, (act_on_pattern p1, e1), (p2, e2))
      | EMatch_E1 (l_child, (p1, e1), (p2, e2)) ->
          EMatch_E1 (l_child, (p1, construct_shape e1), (p2, e2))
      | EMatch_P2 (l_child, (p1, e1), (p2, e2)) ->
          EMatch_P2 (l_child, (p1, e1), (act_on_pattern p2, e2))
      | EMatch_E2 (l_child, (p1, e1), (p2, e2)) ->
          EMatch_E2 (l_child, (p1, e1), (p2, construct_shape e2))
      | EAssert_L child -> EAssert_L (construct_shape child)
      (* at cursor, build the correct expression *)
      | Cursor subtree -> build_expr shape (Expr.unzip tree)
    in
    {
      tree with
      id = (match tree.node with Cursor _ -> Id.generate () | _ -> tree.id);
      node;
    }
  in
  let shuffle_cursor (n_child : int) (subtree : Expr.t) : Expr.z_node =
    (* moves curser to appropriate child of expression in `subtree` *)
    match (n_child, subtree.node) with
    | 0, EUnOp (op, arg) -> EUnOp_L (op, Expr.select_root arg)
    | 0, EBinOp (arg_l, op, arg_r) ->
        EBinOp_L (Expr.select_root arg_l, op, arg_r)
    | 0, ELet (varn, arg_l, arg_r) ->
        ELet_L (varn, Expr.select_root arg_l, arg_r)
    | 0, EIf (arg_l, arg_c, arg_r) ->
        EIf_L (Expr.select_root arg_l, arg_c, arg_r)
    | 0, EFun (varname, typ, arg) -> EFun_L (varname, Type.select_root typ, arg)
    | 0, EFix (varname, typ, arg) -> EFix_L (varname, Type.select_root typ, arg)
    | 0, EPair (arg_l, arg_r) -> EPair_L (Expr.select_root arg_l, arg_r)
    | 0, EMap (arg_l, arg_r) -> EMap_L (Expr.select_root arg_l, arg_r)
    | 0, EFilter (arg_l, arg_r) -> EFilter_L (Expr.select_root arg_l, arg_r)
    | 0, EMatch (arg_l, (p1, e1), (p2, e2)) ->
        EMatch_L (Expr.select_root arg_l, (p1, e1), (p2, e2))
    | 0, EAssert arg -> EAssert_L (Expr.select_root arg)
    | 0, EFold (e1, e2, e3) -> EFold_L (Expr.select_root e1, e2, e3)
    | 1, EBinOp (arg_l, op, arg_r) ->
        EBinOp_R (arg_l, op, Expr.select_root arg_r)
    | 1, ELet (varn, arg_l, arg_r) ->
        ELet_R (varn, arg_l, Expr.select_root arg_r)
    | 1, EIf (arg_l, arg_c, arg_r) ->
        EIf_C (arg_l, Expr.select_root arg_c, arg_r)
    | 1, EPair (arg_l, arg_r) -> EPair_R (arg_l, Expr.select_root arg_r)
    | 1, EMap (arg_l, arg_r) -> EMap_R (arg_l, Expr.select_root arg_r)
    | 1, EFilter (arg_l, arg_r) -> EFilter_R (arg_l, Expr.select_root arg_r)
    | 1, EFold (e1, e2, e3) -> EFold_C (e1, Expr.select_root e2, e3)
    | 1, EFun (varname, typ, arg_l) ->
        EFun_R (varname, typ, Expr.select_root arg_l)
    | 1, EFix (varname, typ, arg_l) ->
        EFix_R (varname, typ, Expr.select_root arg_l)
    | 1, EMatch (arg_l, (p1, e1), (p2, e2)) ->
        EMatch_P1 (arg_l, (Pattern.select_root p1, e1), (p2, e2))
    | 2, EIf (arg_l, arg_c, arg_r) ->
        EIf_R (arg_l, arg_c, Expr.select_root arg_r)
    | 2, EFold (e1, e2, e3) -> EFold_R (e1, e2, Expr.select_root e3)
    | 2, EMatch (arg_l, (p1, e1), (p2, e2)) ->
        EMatch_E1 (arg_l, (p1, Expr.select_root e1), (p2, e2))
    | 3, EMatch (arg_l, (p1, e1), (p2, e2)) ->
        EMatch_P2 (arg_l, (p1, e1), (Pattern.select_root p2, e2))
    | 4, EMatch (arg_l, (p1, e1), (p2, e2)) ->
        EMatch_E2 (arg_l, (p1, e1), (p2, Expr.select_root e2))
    | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
    (*all invalid actions are noops*)
  in
  let rec move_child (n_child : int) (tree : Expr.z_t) : Expr.z_t =
    (* recurses to cursor then moves cursor to appropriate child*)
    let move_n_child = move_child n_child in
    let node : Expr.z_node =
      match tree.node with
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
      | EFold_L (l, c, r) -> EFold_L (move_n_child l, c, r)
      | EFold_C (l, c, r) -> EFold_C (l, move_n_child c, r)
      | EFold_R (l, c, r) -> EFold_R (l, c, move_n_child r)
      | EFun_R (var, typ, child) -> EFun_R (var, typ, move_n_child child)
      | EFun_L (var, typ, child) -> EFun_L (var, act_on_type typ, child)
      | EFix_R (var, typ, child) -> EFix_R (var, typ, move_n_child child)
      | EFix_L (var, typ, child) -> EFix_L (var, act_on_type typ, child)
      | EPair_L (l_child, r_child) -> EPair_L (move_n_child l_child, r_child)
      | EPair_R (l_child, r_child) -> EPair_R (l_child, move_n_child r_child)
      | EMap_L (l_child, r_child) -> EMap_L (move_n_child l_child, r_child)
      | EMap_R (l_child, r_child) -> EMap_R (l_child, move_n_child r_child)
      | EFilter_L (l_child, r_child) -> EFilter_L (move_n_child l_child, r_child)
      | EFilter_R (l_child, r_child) -> EFilter_R (l_child, move_n_child r_child)
      | EListEq_L (l_child, r_child) -> EListEq_L (move_n_child l_child, r_child)
      | EListEq_R (l_child, r_child) -> EListEq_R (l_child, move_n_child r_child)
      | EMatch_L (l, (p1, e1), (p2, e2)) ->
          EMatch_L (move_n_child l, (p1, e1), (p2, e2))
      | EMatch_P1 (l, (p1, e1), (p2, e2)) ->
          EMatch_P1 (l, (act_on_pattern p1, e1), (p2, e2))
      | EMatch_E1 (l, (p1, e1), (p2, e2)) ->
          EMatch_E1 (l, (p1, move_n_child e1), (p2, e2))
      | EMatch_P2 (l, (p1, e1), (p2, e2)) ->
          EMatch_P2 (l, (p1, e1), (act_on_pattern p2, e2))
      | EMatch_E2 (l, (p1, e1), (p2, e2)) ->
          EMatch_E2 (l, (p1, e1), (p2, move_n_child e2))
      | EAssert_L r_child -> EAssert_L (move_n_child r_child)
      (*Once cursor is reached, use dedicated func to move to appropriate subtree*)
      | Cursor subtree -> shuffle_cursor n_child (Expr.unzip tree)
    in
    { tree with node }
  in
  let rec move_parent (tree : Expr.z_t) : Expr.z_t =
    (*Handles move-parent operations.
       for each node type, we have two options:
        if the cursor is a direct child, move it upward
        Otherwise, continue recursing into the tree *)
    let node : Expr.z_node =
      match tree.node with
      | EUnOp_L (op, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EUnOp (op, { (Expr.unzip subtr) with node = arg }))
      | EUnOp_L (op, arg) -> EUnOp_L (op, move_parent arg)
      | EBinOp_L (({ node = Cursor arg; _ } as subtr), op, r_child) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EBinOp ({ (Expr.unzip subtr) with node = arg }, op, r_child))
      | EBinOp_L (l_child, op, r_child) ->
          EBinOp_L (move_parent l_child, op, r_child)
      | EBinOp_R (l_child, op, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EBinOp (l_child, op, { (Expr.unzip subtr) with node = arg }))
      | EBinOp_R (l_child, op, r_child) ->
          EBinOp_R (l_child, op, move_parent r_child)
      | EPair_L (({ node = Cursor arg; _ } as subtr), r_child) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EPair ({ (Expr.unzip subtr) with node = arg }, r_child))
      | EPair_L (l_child, r_child) -> EPair_L (move_parent l_child, r_child)
      | EPair_R (l_child, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EPair (l_child, { (Expr.unzip subtr) with node = arg }))
      | EPair_R (l_child, r_child) -> EPair_R (l_child, move_parent r_child)
      (*working on map*)
      | EMap_L (({ node = Cursor arg; _ } as subtr), r_child) ->
          Cursor (EMap ({ (Expr.unzip subtr) with node = arg }, r_child))
      | EMap_L (l_child, r_child) -> EMap_L (move_parent l_child, r_child)
      | EMap_R (l_child, ({ node = Cursor arg; _ } as subtr)) ->
          Cursor (EMap (l_child, { (Expr.unzip subtr) with node = arg }))
      | EMap_R (l_child, r_child) -> EMap_R (l_child, move_parent r_child)
      (*working on  filter*)
      | EFilter_L (({ node = Cursor arg; _ } as subtr), r_child) ->
          Cursor (EFilter ({ (Expr.unzip subtr) with node = arg }, r_child))
      | EFilter_L (l_child, r_child) -> EFilter_L (move_parent l_child, r_child)
      | EFilter_R (l_child, ({ node = Cursor arg; _ } as subtr)) ->
          Cursor (EFilter (l_child, { (Expr.unzip subtr) with node = arg }))
      | EFilter_R (l_child, r_child) -> EFilter_R (l_child, move_parent r_child)
      (* let etc *)
      | ELet_L (var, ({ node = Cursor arg; _ } as subtr), r_child) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor (ELet (var, { (Expr.unzip subtr) with node = arg }, r_child))
      | ELet_L (var, l_child, r_child) ->
          ELet_L (var, move_parent l_child, r_child)
      | ELet_R (var, l_child, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor (ELet (var, l_child, { (Expr.unzip subtr) with node = arg }))
      | ELet_R (var, l_child, r_child) ->
          ELet_R (var, l_child, move_parent r_child)
      | EIf_L (({ node = Cursor arg; _ } as subtr), c, r) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EIf ({ (Expr.unzip subtr) with node = arg }, c, r))
      | EIf_L (l, c, r) -> EIf_L (move_parent l, c, r)
      | EIf_C (l, ({ node = Cursor arg; _ } as subtr), r) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EIf (l, { (Expr.unzip subtr) with node = arg }, r))
      | EIf_C (l, c, r) -> EIf_C (l, move_parent c, r)
      | EIf_R (l, c, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EIf (l, c, { (Expr.unzip subtr) with node = arg }))
      | EIf_R (l, c, r) -> EIf_R (l, c, move_parent r)
      (* working on Fold*)
      | EFold_L (({ node = Cursor arg; _ } as subtr), c, r) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EFold ({ (Expr.unzip subtr) with node = arg }, c, r))
      | EFold_L (l, c, r) -> EFold_L (move_parent l, c, r)
      | EFold_C (l, ({ node = Cursor arg; _ } as subtr), r) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EFold (l, { (Expr.unzip subtr) with node = arg }, r))
      | EFold_C (l, c, r) -> EFold_C (l, move_parent c, r)
      | EFold_R (l, c, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EFold (l, c, { (Expr.unzip subtr) with node = arg }))
      | EFold_R (l, c, r) -> EFold_R (l, c, move_parent r)
      (* done working on fold*)
      | EFun_L (var, ({ node = Cursor arg; _ } as subtr), ebody) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor (EFun (var, { (Type.unzip subtr) with node = arg }, ebody))
      | EFun_L (var, typ, arg) -> EFun_L (var, act_on_type typ, arg)
      | EFix_L (var, ({ node = Cursor arg; _ } as subtr), ebody) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor (EFix (var, { (Type.unzip subtr) with node = arg }, ebody))
      | EFix_L (var, typ, arg) -> EFix_L (var, act_on_type typ, arg)
      | EFun_R (var, typ, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EFun (var, typ, { (Expr.unzip subtr) with node = arg }))
      | EFun_R (var, typ, child) -> EFun_R (var, typ, move_parent child)
      | EFix_R (var, typ, ({ node = Cursor arg; _ } as subtr)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EFix (var, typ, { (Expr.unzip subtr) with node = arg }))
      | EFix_R (var, typ, child) -> EFix_R (var, typ, move_parent child)
      | EMatch_L (({ node = Cursor arg; _ } as subtr), (p1, e1), (p2, e2)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EMatch
                 ({ (Expr.unzip subtr) with node = arg }, (p1, e1), (p2, e2)))
      | EMatch_L (e, (p1, e1), (p2, e2)) ->
          EMatch_L (move_parent e, (p1, e1), (p2, e2))
      | EMatch_P1 (e, (({ node = Cursor arg; _ } as subtr), e1), (p2, e2)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EMatch
                 (e, ({ (Pattern.unzip subtr) with node = arg }, e1), (p2, e2)))
      | EMatch_P1 (e, (p1, e1), (p2, e2)) ->
          EMatch_P1 (e, (act_on_pattern p1, e1), (p2, e2))
      | EMatch_E1 (e, (p1, ({ node = Cursor arg; _ } as subtr)), (p2, e2)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EMatch (e, (p1, { (Expr.unzip subtr) with node = arg }), (p2, e2)))
      | EMatch_E1 (e, (p1, e1), (p2, e2)) ->
          EMatch_E1 (e, (p1, move_parent e1), (p2, e2))
      | EMatch_P2 (e, (p1, e1), (({ node = Cursor arg; _ } as subtr), e2)) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EMatch
                 (e, (p1, e1), ({ (Pattern.unzip subtr) with node = arg }, e2)))
      | EMatch_P2 (e, (p1, e1), (p2, e2)) ->
          EMatch_P2 (e, (p1, e1), (act_on_pattern p2, e2))
      | EMatch_E2 (l, (p1, e1), (p2, ({ node = Cursor arg; _ } as subtr))) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else
            Cursor
              (EMatch (l, (p1, e1), (p2, { (Expr.unzip subtr) with node = arg })))
      | EMatch_E2 (e, (p1, e1), (p2, e2)) ->
          EMatch_E2 (e, (p1, e1), (p2, move_parent e2))
      | EAssert_L ({ node = Cursor arg; _ } as subtr) ->
          if tree.starter
          then raise (InvalidAction (ActionConv.action_to_tag action))
          else Cursor (EAssert { (Expr.unzip subtr) with node = arg })
      | EAssert_L child -> EAssert_L (move_parent child)
      | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
    in
    { tree with node }
  in
  let rec unwrap (tree : Expr.z_t) (n : int) : Expr.z_t =
    let rec check_var (e : Expr.t) (x : Var.t) : bool =
      match e.node with
      | EVar v -> not (Var.equal x v)
      | EHole | EConst _ -> true
      | EUnOp (_, e) | EFun (_, _, e) | EFix (_, _, e) | EAssert e ->
          check_var e x
      | EBinOp (e1, _, e2)
      | EPair (e1, e2)
      | ELet (_, e1, e2)
      | EMap (e1, e2)
      | EFilter (e1, e2)
      | EListEq (e1, e2) ->
          check_var e1 x && check_var e2 x
      | EFold (e1, e2, e3) | EIf (e1, e2, e3) ->
          check_var e1 x && check_var e2 x && check_var e3 x
      | EMatch (e, (p1, e1), (p2, e2)) ->
          check_var e x && check_var e1 x && check_var e2 x
    in
    match tree.node with
    | Cursor e ->
        let subtree =
          match (n, e) with
          | 0, EUnOp (_, e) -> e
          | 0, EBinOp (e, _, _) -> e
          | 1, EBinOp (_, _, e) -> e
          | 0, ELet (_, e, _) -> e
          | 1, ELet (x, _, e) ->
              if check_var e x
              then e
              else raise (InvalidAction (ActionConv.action_to_tag action))
          | 0, EIf (e, _, _) -> e
          | 1, EIf (_, e, _) -> e
          | 2, EIf (_, _, e) -> e
          | 1, EFun (x, _, e) ->
              if check_var e x
              then e
              else raise (InvalidAction (ActionConv.action_to_tag action))
          | 1, EFix (x, _, e) ->
              if check_var e x
              then e
              else raise (InvalidAction (ActionConv.action_to_tag action))
          | 0, EPair (e, _) -> e
          | 1, EPair (_, e) -> e
          | 0, EMap (e, _) -> e
          | 1, EMap (_, e) -> e
          | 0, EFold (e, _, _) -> e
          | 1, EFold (_, e, _) -> e
          | 2, EFold (_, _, e) -> e
          | 0, EFilter (e, _) -> e
          | 1, EFilter (_, e) -> e
          | 0, EMatch (e, _, _) -> e
          | 1, EMatch (_, (p, e), _) -> (
              match p.node with
              | PVar x ->
                  if check_var e x
                  then e
                  else raise (InvalidAction (ActionConv.action_to_tag action))
              | _ -> e)
          | 2, EMatch (_, (_, _), (p, e)) -> (
              match p.node with
              | PVar x ->
                  if check_var e x
                  then e
                  else raise (InvalidAction (ActionConv.action_to_tag action))
              | _ -> e)
          | 0, EAssert e -> e
          | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        in
        Expr.select_root subtree
    | _ ->
        let node : Expr.z_node =
          match tree.node with
          | EUnOp_L (op, r_child) -> EUnOp_L (op, unwrap r_child n)
          | EBinOp_L (l_child, op, r_child) ->
              EBinOp_L (unwrap l_child n, op, r_child)
          | EBinOp_R (l_child, op, r_child) ->
              EBinOp_R (l_child, op, unwrap r_child n)
          | ELet_L (var, l_child, r_child) ->
              ELet_L (var, unwrap l_child n, r_child)
          | ELet_R (var, l_child, r_child) ->
              ELet_R (var, l_child, unwrap r_child n)
          | EIf_L (l, c, r) -> EIf_L (unwrap l n, c, r)
          | EIf_C (l, c, r) -> EIf_C (l, unwrap c n, r)
          | EIf_R (l, c, r) -> EIf_R (l, c, unwrap r n)
          | EFold_L (l, c, r) -> EFold_L (unwrap l n, c, r)
          | EFold_C (l, c, r) -> EFold_C (l, unwrap c n, r)
          | EFold_R (l, c, r) -> EFold_R (l, c, unwrap r n)
          | EFun_R (var, typ, child) -> EFun_R (var, typ, unwrap child n)
          | EFix_R (var, typ, child) -> EFix_R (var, typ, unwrap child n)
          | EPair_L (l_child, r_child) -> EPair_L (unwrap l_child n, r_child)
          | EPair_R (l_child, r_child) -> EPair_R (l_child, unwrap r_child n)
          | EMap_L (l_child, r_child) -> EMap_L (unwrap l_child n, r_child)
          | EMap_R (l_child, r_child) -> EMap_R (l_child, unwrap r_child n)
          | EFilter_L (l_child, r_child) -> EFilter_L (unwrap l_child n, r_child)
          | EFilter_R (l_child, r_child) -> EFilter_R (l_child, unwrap r_child n)
          | EMatch_L (l, (p1, e1), (p2, e2)) ->
              EMatch_L (unwrap l n, (p1, e1), (p2, e2))
          | EMatch_P1 (l, (p1, e1), (p2, e2)) ->
              EMatch_P1 (l, (act_on_pattern p1, e1), (p2, e2))
          | EMatch_E1 (l, (p1, e1), (p2, e2)) ->
              EMatch_E1 (l, (p1, unwrap e1 n), (p2, e2))
          | EMatch_P2 (l, (p1, e1), (p2, e2)) ->
              EMatch_P2 (l, (p1, e1), (act_on_pattern p2, e2))
          | EMatch_E2 (l, (p1, e1), (p2, e2)) ->
              EMatch_E2 (l, (p1, e1), (p2, unwrap e2 n))
          | EAssert_L e -> EAssert_L (unwrap e n)
          | _ -> raise (InvalidAction (ActionConv.action_to_tag action))
        in
        { tree with node }
  in
  let act_on (tree : Expr.z_t) : Expr.z_t =
    match action with
    | Construct shape -> construct shape tree
    | Unwrap n -> unwrap tree n
    | Move (Child n) -> move_child n tree
    | Move Parent -> move_parent tree
  in
  act_on tree

(* let%test_module "Test perform_action" =
   (module struct
     let%test _ =
       Expr.z_equal
         (Cursor (EBinOp (EInt 2, OpPlus, EInt 3)))
         (perform_action
            (EBinOp_L (Cursor (EInt 2), OpPlus, EInt 3))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EBinOp (EInt 2, OpDiv, EInt 3)))
         (perform_action
            (EBinOp_R (EInt 2, OpDiv, Cursor (EInt 3)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EUnOp (OpNeg, EBool false)))
         (perform_action (EUnOp_L (OpNeg, Cursor (EBool false))) (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (ELet ("var", EInt 2, EHole)))
         (perform_action (ELet_L ("var", Cursor (EInt 2), EHole)) (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (ELet ("var", EInt 2, EHole)))
         (perform_action (ELet_R ("var", EInt 2, Cursor EHole)) (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
         (perform_action
            (EIf_L (Cursor (EBool false), EBinOp (EInt 3, OpPlus, EInt 2), EInt 3))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
         (perform_action
            (EIf_C (EBool false, Cursor (EBinOp (EInt 3, OpPlus, EInt 2)), EInt 3))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
         (perform_action
            (EIf_R (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), Cursor (EInt 3)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (EFun_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (EFix_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EPair (EBool false, EInt 3)))
         (perform_action (EPair_L (Cursor (EBool false), EInt 3)) (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EPair (EBool false, EInt 3)))
         (perform_action (EPair_R (EBool false, Cursor (EInt 3))) (Move Parent))
       = true

     (* test ability to 'recurse through' each type *)
     let%test _ =
       Expr.z_equal
         (EUnOp_L (OpNeg, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (EUnOp_L (OpNeg, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_L (Cursor (EUnOp (OpNeg, EHole)), OpLe, EHole))
         (perform_action
            (EBinOp_L (EUnOp_L (OpNeg, Cursor EHole), OpLe, EHole))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_R (EHole, OpGt, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (EBinOp_R (EHole, OpGt, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_L ("name", Cursor (EUnOp (OpNeg, EHole)), EHole))
         (perform_action
            (ELet_L ("name", EUnOp_L (OpNeg, Cursor EHole), EHole))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_R ("name", EHole, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (ELet_R ("name", EHole, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_L (Cursor (EUnOp (OpNeg, EHole)), EHole, ENil))
         (perform_action
            (EIf_L (EUnOp_L (OpNeg, Cursor EHole), EHole, ENil))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_C (EBool false, Cursor (EUnOp (OpNeg, EHole)), EHole))
         (perform_action
            (EIf_C (EBool false, EUnOp_L (OpNeg, Cursor EHole), EHole))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_R (EBool false, EHole, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (EIf_R (EBool false, EHole, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (EFun_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFun ("var", TList TBool, EUnOp (OpNeg, EHole))))
         (perform_action
            (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
         (perform_action
            (EFix_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (Cursor (EFix ("var", TList TBool, EUnOp (OpNeg, EHole))))
         (perform_action
            (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EPair_L (Cursor (EUnOp (OpNeg, EHole)), ENil))
         (perform_action
            (EPair_L (EUnOp_L (OpNeg, Cursor EHole), ENil))
            (Move Parent))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_L (Cursor (EInt 2), OpPlus, EInt 3))
         (perform_action
            (Cursor (EBinOp (EInt 2, OpPlus, EInt 3)))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_R (EInt 2, OpDiv, Cursor (EInt 3)))
         (perform_action
            (Cursor (EBinOp (EInt 2, OpDiv, EInt 3)))
            (Move (Child 1)))
       = true

     let%test _ =
       Expr.z_equal
         (EUnOp_L (OpNeg, Cursor (EBool false)))
         (perform_action (Cursor (EUnOp (OpNeg, EBool false))) (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_L ("var", Cursor (EInt 2), EHole))
         (perform_action (Cursor (ELet ("var", EInt 2, EHole))) (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_R ("var", EInt 2, Cursor EHole))
         (perform_action (Cursor (ELet ("var", EInt 2, EHole))) (Move (Child 1)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_L (Cursor (EBool false), EBinOp (EInt 3, OpPlus, EInt 2), EInt 3))
         (perform_action
            (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_C (EBool false, Cursor (EBinOp (EInt 3, OpPlus, EInt 2)), EInt 3))
         (perform_action
            (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
            (Move (Child 1)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_R (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), Cursor (EInt 3)))
         (perform_action
            (Cursor (EIf (EBool false, EBinOp (EInt 3, OpPlus, EInt 2), EInt 3)))
            (Move (Child 2)))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
         (perform_action
            (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
            (Move (Child 1)))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
         (perform_action
            (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
         (perform_action
            (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
            (Move (Child 1)))
       = true

     let%test _ =
       Expr.z_equal
         (EPair_L (Cursor (EBool false), EInt 3))
         (perform_action (Cursor (EPair (EBool false, EInt 3))) (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EPair_R (EBool false, Cursor (EInt 3)))
         (perform_action (Cursor (EPair (EBool false, EInt 3))) (Move (Child 1)))
       = true

     (* test ability to 'recurse through' each type *)
     let%test _ =
       Expr.z_equal
         (EUnOp_L (OpNeg, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EUnOp_L (OpNeg, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_L (EUnOp_L (OpNeg, Cursor EHole), OpLe, EHole))
         (perform_action
            (EBinOp_L (Cursor (EUnOp (OpNeg, EHole)), OpLe, EHole))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EBinOp_R (EHole, OpGt, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EBinOp_R (EHole, OpGt, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_L ("name", EUnOp_L (OpNeg, Cursor EHole), EHole))
         (perform_action
            (ELet_L ("name", Cursor (EUnOp (OpNeg, EHole)), EHole))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (ELet_R ("name", EHole, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (ELet_R ("name", EHole, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_L (EUnOp_L (OpNeg, Cursor EHole), EHole, ENil))
         (perform_action
            (EIf_L (Cursor (EUnOp (OpNeg, EHole)), EHole, ENil))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_C (EBool false, EUnOp_L (OpNeg, Cursor EHole), EHole))
         (perform_action
            (EIf_C (EBool false, Cursor (EUnOp (OpNeg, EHole)), EHole))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EIf_R (EBool false, EHole, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EIf_R (EBool false, EHole, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_L ("var", List_L (Cursor TBool), EUnOp (OpNeg, EHole)))
         (perform_action
            (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
         (perform_action
            (Cursor (EFun ("var", TList TBool, EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_L ("var", List_L (Cursor TBool), EUnOp (OpNeg, EHole)))
         (perform_action
            (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
         (perform_action
            (Cursor (EFix ("var", TList TBool, EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EPair_L (EUnOp_L (OpNeg, Cursor EHole), ENil))
         (perform_action
            (EPair_L (Cursor (EUnOp (OpNeg, EHole)), ENil))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (EPair_R (EHole, EUnOp_L (OpNeg, Cursor EHole)))
         (perform_action
            (EPair_R (EHole, Cursor (EUnOp (OpNeg, EHole))))
            (Move (Child 0)))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action (Cursor EHole) (Construct (Var "x")))
         (Cursor (EVar "x"))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action (Cursor EHole) (Construct (Var "y")))
         (Cursor (EVar "y"))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action (EUnOp_L (OpNeg, Cursor ENil)) (Construct Hole))
         (EUnOp_L (OpNeg, Cursor EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action (EBinOp_R (EInt 2, OpGt, Cursor EHole)) (Construct Nil))
         (EBinOp_R (EInt 2, OpGt, Cursor ENil))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EBinOp_L (Cursor EHole, OpEq, EInt 2))
            (Construct (Int 4)))
         (EBinOp_L (Cursor (EInt 4), OpEq, EInt 2))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (ELet_L ("varname", Cursor EHole, EInt 2))
            (Construct (Bool false)))
         (ELet_L ("varname", Cursor (EBool false), EInt 2))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (ELet_R ("varname", EInt 2, Cursor (EVar "myvar")))
            (Construct (UnOp OpNeg)))
         (ELet_R ("varname", EInt 2, Cursor (EUnOp (OpNeg, EVar "myvar"))))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EIf_L (Cursor (EVar "myvar"), EInt 2, EInt 3))
            (Construct (BinOp_L OpNe)))
         (EIf_L (Cursor (EBinOp (EVar "myvar", OpNe, EHole)), EInt 2, EInt 3))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EIf_C (EBool false, Cursor (EVar "myvar1"), EInt 3))
            (Construct (BinOp_R OpEq)))
         (EIf_C
            (EBool false, Cursor (EBinOp (EHole, OpEq, EVar "myvar1")), EInt 3))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EIf_R (EBool false, EInt 2, Cursor (EVar "myvar")))
            (Construct (Let_L "newvar")))
         (EIf_R
            (EBool false, EInt 2, Cursor (ELet ("newvar", EVar "myvar", EHole))))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_R ("fvar", TBool, Cursor (EVar "myvar")))
            (Construct (Let_R "newvar")))
         (EFun_R ("fvar", TBool, Cursor (ELet ("newvar", EHole, EVar "myvar"))))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFix_R ("fvar", TBool, Cursor (EVar "myvar")))
            (Construct If_L))
         (EFix_R ("fvar", TBool, Cursor (EIf (EVar "myvar", EHole, EHole))))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_R (EInt 0, Cursor (EVar "myvar")))
            (Construct If_C))
         (EPair_R (EInt 0, Cursor (EIf (EHole, EVar "myvar", EHole))))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_L (Cursor (EVar "myvar"), EInt 0))
            (Construct If_R))
         (EPair_L (Cursor (EIf (EHole, EHole, EVar "myvar")), EInt 0))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_L (Cursor (EVar "myvar"), EInt 0))
            (Construct (Fun "newvar")))
         (EPair_L (Cursor (EFun ("newvar", THole, EVar "myvar")), EInt 0))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_L (Cursor (EVar "myvar"), EInt 0))
            (Construct (Fix "newvar")))
         (EPair_L (Cursor (EFix ("newvar", THole, EVar "myvar")), EInt 0))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_L (Cursor (EVar "myvar"), EInt 0))
            (Construct Pair_L))
         (EPair_L (Cursor (EPair (EVar "myvar", EHole)), EInt 0))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EPair_L (Cursor (EVar "myvar"), EInt 0))
            (Construct Pair_R))
         (EPair_L (Cursor (EPair (EHole, EVar "myvar")), EInt 0))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TBool, EHole))
            (Construct TypInt))
         (EFun_L ("newvar", Cursor TInt, EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TInt, EHole))
            (Construct TypInt))
         (EFun_L ("newvar", Cursor TInt, EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TBool, EHole))
            (Construct TypArrow_L))
         (EFun_L ("newvar", Cursor (TArrow (TBool, THole)), EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TInt, EHole))
            (Construct TypArrow_R))
         (EFun_L ("newvar", Cursor (TArrow (THole, TInt)), EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFix_L ("newvar", Cursor TBool, EHole))
            (Construct TypProd_L))
         (EFix_L ("newvar", Cursor (TProd (TBool, THole)), EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFix_L ("newvar", Type.Cursor TInt, EHole))
            (Construct TypProd_R))
         (EFix_L ("newvar", Type.Cursor (TProd (THole, TInt)), EHole))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TBool, EVar "hey"))
            (Construct TypList))
         (EFun_L ("newvar", Cursor (TList TBool), EVar "hey"))
       = true

     let%test _ =
       Expr.z_equal
         (perform_action
            (EFun_L ("newvar", Cursor TBool, EVar "hey"))
            (Construct TypHole))
         (EFun_L ("newvar", Cursor THole, EVar "hey"))
       = true
   end) *)

let check_actions (actions : Action.t list) (e : Expr.z_t) : Action.t list =
  let check_action (action : Action.t) : bool =
    let used_vars = Array.copy Var.used_vars in
    let e' = try Some (perform_action e action) with _ -> None in
    Var.copy_vars used_vars;
    match e' with
    | Some e' -> (
        let t =
          try Typing.synthesis Context.empty (Expr.unzip e') with _ -> None
        in
        match t with Some _ -> true | None -> false)
    | None -> false
  in
  List.filter check_action actions
