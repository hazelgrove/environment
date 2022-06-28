(* Information at the cursor *)
open Typing

type t = {
  current_term : Syntax.t;
  (*the currently focussed term (use to decide whether we can go down) *)
  (*is_root: bool; (*boolean value of whether or not cursor is at root. simpler version of vv*)  *)
  parent_term : Syntax.t option;
  (* parent of current term (use to decide whether we can go up)  *)
  ctx : (Var.t * int) list;
  (* variable types *)
  typ_ctx : (Var.t * Type.t) list;
  (*mapping of vars in scope to types (use to determine vars in scope)    *)
  expected_ty : Type.t option;
  (* analyzed type of cursor_term; build up through recursion (use with ctx to determine viable insert actions) *)
  actual_ty : Type.t option;
      (* result of calling Syn on current_term (use to determine wrapping viability)  *)
}

(* Given a zippered AST, return the information (expr, typing, etc.) at the cursor *)
let get_cursor_info (e : Syntax.z_t) : t =
  let rec recurse (node : Syntax.z_t) (parent : Syntax.t option)
      (def_cont : (Var.t * int) list) (typ_cont : Context.t)
      (pred_type : Type.t option) (ind : int) : t =
    let current : Syntax.t =
      match node with
      | ZENode zparent -> ENode (Expr.unzip_ast zparent)
      | ZTNode zparent -> TNode (Type.unzip zparent)
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
          (ind + Syntax.size (ENode argl) + 1)
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
          (ind + Syntax.size (ENode argl) + 1)
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
              (ind + Syntax.size (ENode argl) + 1)
        | _ ->
            recurse (ZENode argr) (Some current) def_cont typ_cont None
              (ind + Syntax.size (ENode argl) + 1))
    | ZENode (ELet_L (_, argl, argr)) ->
        recurse (ZENode argl) (Some current) def_cont typ_cont None ind
    | ZENode (ELet_R (varn, argl, argr)) -> (
        match synthesis typ_cont argl with
        | Some lettype ->
            recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
              ((varn, lettype) :: typ_cont)
              None
              (ind + Syntax.size (ENode argl) + 1)
        | None ->
            recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
              ((varn, THole) :: typ_cont)
              None
              (ind + Syntax.size (ENode argl) + 1))
    | ZENode (EIf_L (argl, argc, argr)) ->
        recurse (ZENode argl) (Some current) def_cont typ_cont (Some TBool) ind
    | ZENode (EIf_C (argl, argc, argr)) ->
        recurse (ZENode argc) (Some current) def_cont typ_cont
          (synthesis typ_cont argr)
          (ind + Syntax.size (ENode argl) + 1)
    | ZENode (EIf_R (argl, argc, argr)) ->
        recurse (ZENode argr) (Some current) def_cont typ_cont
          (synthesis typ_cont argc)
          (ind + Syntax.size (ENode argl) + Syntax.size (ENode argc) + 1)
    | ZENode (EFun_L (_, typ, argr)) ->
        recurse (ZTNode typ) (Some current) def_cont typ_cont None ind
    | ZENode (EFun_R (varn, typ, argr)) ->
        recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
          ((varn, typ) :: typ_cont) None
          (ind + Syntax.size (TNode typ) + 1)
    | ZENode (EFix_L (_, typ, argr)) ->
        recurse (ZTNode typ) (Some current) def_cont typ_cont None ind
    | ZENode (EFix_R (varn, typ, argr)) ->
        recurse (ZENode argr) (Some current) ((varn, ind) :: def_cont)
          ((varn, typ) :: typ_cont) None
          (ind + Syntax.size (TNode typ) + 1)
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
              (ind + Syntax.size (ENode argl) + 1)
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
          (ind + Syntax.size (TNode in_typ) + 1)
    | ZTNode (Prod_L (l_typ, r_typ)) ->
        recurse (ZTNode l_typ) (Some current) def_cont typ_cont None ind
    | ZTNode (Prod_R (l_typ, r_typ)) ->
        recurse (ZTNode r_typ) (Some current) def_cont typ_cont None
          (ind + Syntax.size (TNode l_typ) + 1)
    | ZTNode (List_L l_typ) ->
        recurse (ZTNode l_typ) (Some current) def_cont typ_cont None ind
  in
  recurse e None [] [] None 0

let rec get_cursor_position (tree : Syntax.z_t) =
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
          1 + 1 + Type.size ty + get_cursor_position (ZENode e))
  | ZTNode ty -> (
      match ty with
      | Cursor _ -> 0
      | Arrow_L (ty, _) | Prod_L (ty, _) -> 1 + get_cursor_position (ZTNode ty)
      | Arrow_R (t1, t2) | Prod_R (t1, t2) ->
          1 + Type.size t1 + get_cursor_position (ZTNode t2)
      | List_L ty -> 1 + get_cursor_position (ZTNode ty))

let%test_module "Test get_cursor_position" =
  (module struct
    let parse = ParserUtils.parse
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

(* Given the info at the cursor, return a list of possible actions *)
let cursor_info_to_actions (info : t) : Action.t list =
  let handle_root (ci : t) (currlist : Action.t list) : Action.t list =
    match ci.parent_term with
    | Some _ -> Move Parent :: currlist
    | None -> currlist
  in
  let open Action in
  let handle_children (currlist : Action.t list) : Action.t list =
    match info.current_term with
    | ENode (EIf _) ->
        [ Move (Child 0); Move (Child 1); Move (Child 2) ] @ currlist
    | ENode (EBinOp _ | EFun _ | ELet _ | EFix _ | EPair _)
    | TNode (TArrow _ | TProd _) ->
        [ Move (Child 0); Move (Child 1) ] @ currlist
    | ENode (EUnOp _) | TNode (TList _) -> Move (Child 0) :: currlist
    | ENode (EVar _ | EInt _ | EBool _ | EHole | ENil)
    | TNode (TInt | TBool | THole) ->
        currlist
  in
  let handle_constr_typ (currlist : Action.t list) : Action.t list =
    [
      Construct TypInt;
      Construct TypBool;
      Construct TypArrow_L;
      Construct TypArrow_R;
      Construct TypList;
      Construct TypHole;
      Construct TypProd_L;
      Construct TypProd_R;
    ]
    @ currlist
  in
  let handle_constr_arithmetic_binops (currlist : Action.t list) : Action.t list
      =
    match (info.expected_ty, info.actual_ty) with
    | None, None (* always allow none none's *) | Some TInt, Some TInt ->
        [
          Construct (BinOp_L OpPlus);
          Construct (BinOp_L OpMinus);
          Construct (BinOp_L OpTimes);
          Construct (BinOp_L OpDiv);
          Construct (BinOp_R OpPlus);
          Construct (BinOp_R OpMinus);
          Construct (BinOp_R OpTimes);
          Construct (BinOp_R OpDiv);
        ]
        @ currlist
    | _ -> currlist
  in
  let handle_comp_binops (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | None, None (* always allow none none's *) | Some TBool, Some TInt ->
        [
          Construct (BinOp_L OpLt);
          Construct (BinOp_R OpLt);
          Construct (BinOp_L OpLe);
          Construct (BinOp_R OpLe);
          Construct (BinOp_L OpGt);
          Construct (BinOp_R OpGt);
          Construct (BinOp_L OpGe);
          Construct (BinOp_R OpGe);
        ]
        @ currlist
    | _ -> currlist
  in
  let handle_eq_binops (currlist : Action.t list) : Action.t list =
    match info.expected_ty with
    | None | Some TBool ->
        [
          Construct (BinOp_L OpEq);
          Construct (BinOp_L OpNe);
          Construct (BinOp_R OpEq);
          Construct (BinOp_R OpNe);
        ]
        @ currlist
    | _ -> currlist
  in
  let handle_cons_binop (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TList a), Some (TList b) ->
        if Type.equal b THole
        then Construct (BinOp_R OpCons) :: currlist
        else if Type.equal a b
        then
          [ Construct (BinOp_R OpCons); Construct (BinOp_L OpCons) ] @ currlist
        else currlist
    | Some (TList a), Some b ->
        if Type.equal a b
        then Construct (BinOp_L OpCons) :: currlist
        else currlist
    | None, Some (TList _) ->
        [ Construct (BinOp_R OpCons); Construct (BinOp_L OpCons) ] @ currlist
    | None, Some _ -> Construct (BinOp_L OpCons) :: currlist
    | _ -> currlist
  in
  let handle_ap_binop (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some c, Some (TArrow (_, b)) ->
        if Type.equal c b
        then Construct (BinOp_L OpAp) :: currlist
        else currlist
    | Some c, Some b -> Construct (BinOp_R OpAp) :: currlist
    | None, Some (TArrow _) ->
        [ Construct (BinOp_R OpAp); Construct (BinOp_L OpAp) ] @ currlist
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
        [
          Construct (Int 0);
          Construct (Bool false);
          Construct Hole;
          Construct Nil;
        ]
        @ currlist
    | _ -> currlist
  in
  let handle_var (currlist : Action.t list) : Action.t list =
    let valid_vars =
      match info.expected_ty with
      | Some targ_type ->
          List.filter (fun (var, typ) -> Type.equal typ targ_type) info.typ_ctx
      | None -> info.typ_ctx
    in
    List.map (fun (varn, typ) -> Action.Construct (Var varn)) valid_vars
    @ currlist
  in
  let handle_let (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    (* afik we can make let's anywhere*)
    | Some a, Some b ->
        if Type.equal a b
        then [ Construct (Let_L ""); Construct (Let_R "") ] @ currlist
        else Construct (Let_L "") :: currlist
    | _ -> [ Construct (Let_L ""); Construct (Let_R "") ] @ currlist
  in
  let handle_if (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | _, Some TBool ->
        [ Construct If_L; Construct If_C; Construct If_R ] @ currlist
    | _ -> [ Construct If_C; Construct If_R ] @ currlist
  in
  let handle_fun (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TArrow (a, b)), Some c ->
        if Type.equal b c then Construct (Fun "") :: currlist else currlist
    | None, _ -> Construct (Fun "") :: currlist
    | _ -> currlist
  in
  let handle_fix (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TArrow (a, b)), Some c ->
        if Type.equal b c then Construct (Fix "") :: currlist else currlist
    | None, _ -> Construct (Fix "") :: currlist
    | _ -> currlist
  in
  let handle_pair (currlist : Action.t list) : Action.t list =
    match (info.expected_ty, info.actual_ty) with
    | Some (TProd (a, b)), Some c ->
        let left_half =
          if Type.equal a c
          then Action.Construct Pair_L :: currlist
          else currlist
        in
        if Type.equal b c then Construct Pair_R :: left_half else left_half
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
      let _ = handle_cons_binop currlist in
      let currlist = handle_ap_binop currlist in
      let currlist = handle_basic_types currlist in
      let currlist = handle_var currlist in
      let currlist = handle_let currlist in
      let currlist = handle_if currlist in
      let currlist = handle_fun currlist in
      let currlist = handle_fix currlist in
      handle_pair currlist

let%test_module "Test cursor_info_to_actions" =
  (module struct
    let check (e : Expr.z_t) (lst : Action.t list) =
      let actions =
        Syntax.ZENode e |> get_cursor_info |> cursor_info_to_actions
      in
      let rec eq l1 l2 =
        match l1 with
        | [] -> true
        | hd :: tl -> if List.mem hd l2 then eq tl l2 else false
      in
      eq actions lst

    open Action

    (* let rec move_child_actions n =
       if n = -1 then [] else (Move (Child n)) :: (move_child_actions (n - 1)) *)
    let ints =
      [
        Construct (Int (-2));
        Construct (Int (-1));
        Construct (Int 0);
        Construct (Int 1);
        Construct (Int 2);
      ]

    let bools = [ Construct (Bool true); Construct (Bool false) ]

    let arith =
      [
        Construct (BinOp_L OpPlus);
        Construct (BinOp_L OpMinus);
        Construct (BinOp_L OpTimes);
        Construct (BinOp_L OpDiv);
        Construct (BinOp_R OpPlus);
        Construct (BinOp_R OpMinus);
        Construct (BinOp_R OpTimes);
        Construct (BinOp_R OpDiv);
      ]

    let comp =
      [
        Construct (BinOp_L OpLt);
        Construct (BinOp_L OpLe);
        Construct (BinOp_L OpGt);
        Construct (BinOp_L OpGe);
        Construct (BinOp_L OpEq);
        Construct (BinOp_L OpNe);
        Construct (BinOp_R OpLt);
        Construct (BinOp_R OpLe);
        Construct (BinOp_R OpGt);
        Construct (BinOp_R OpGe);
        Construct (BinOp_R OpEq);
        Construct (BinOp_R OpNe);
      ]

    let e = Expr.Cursor (EInt 1)

    let lst =
      [
        Construct Hole;
        Construct Nil;
        Construct (UnOp OpNeg);
        Construct (BinOp_R OpAp);
        Construct (Let_L Var.undef_var);
        Construct (Let_R Var.undef_var);
        Construct If_C;
        Construct If_R;
        Construct (Fun Var.undef_var);
        Construct Pair_L;
        Construct Pair_R;
      ]
      @ ints @ bools @ arith @ comp

    let%test _ = check e lst

    let e = Expr.EBinOp_L (Cursor EHole, OpPlus, EInt 2)

    let lst =
      [
        Move Parent;
        Construct Hole;
        Construct (UnOp OpNeg);
        Construct (BinOp_L OpAp);
        Construct (BinOp_R OpAp);
        Construct (Let_L Var.undef_var);
        Construct (Let_R Var.undef_var);
        Construct If_L;
        Construct If_C;
        Construct If_R;
      ]
      @ ints @ arith

    let%test _ = check e lst
  end)
