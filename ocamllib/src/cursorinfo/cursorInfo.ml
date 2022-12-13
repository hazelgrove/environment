(* Information at the cursor *)
open Typing

exception TypeError of string

type t = {
  current_term : Syntax.t;
  (*the currently focussed term (use to decide whether we can go down) *)
  parent_term : Syntax.z_t option;
  (* parent of current term (use to decide whether we can go up)  *)
  vars_in_scope : (Var.t * int) list;
  (* variables in scope: (variable_name, index of node) *)
  args_in_scope : (Var.t * int * int) list;
  (* arguments in scope: (variable_name, function_index, argument_index) *)
  typ_ctx : Context.t;
  (*mapping of vars in scope to types (use to determine vars in scope)    *)
  expected_ty : Type.p_t option;
  (* analyzed type of cursor_term; build up through recursion (use with ctx to determine viable insert actions) *)
  actual_ty : Type.p_t option;
  (* result of calling Syn on current_term (use to determine wrapping viability)  *)
  cursor_position : int; (* Index of the cursor *)
  num_nodes : int; (* number of nodes in the tree *)
}

(*
   Given a zippered AST, return the information (expr, typing, etc.) at the cursor
   For variables in scope, assumes that there is no shadowing
*)
let get_cursor_info (tree : Syntax.z_t) : t =
  let rec get_cursor_info_type ~(current_term : Type.z_t)
      ~(parent_term : Syntax.z_t option) ~(index : int) =
    match current_term.node with
    (* TODO: Add exp_ty & actual_ty *)
    | Cursor t ->
        let t : Type.t = { (Type.unzip current_term) with node = t } in
        (* No variables for types, so vars_in_scope & typ_ctx are [] *)
        {
          current_term = TNode t;
          parent_term;
          vars_in_scope = [];
          args_in_scope = [];
          typ_ctx = [];
          expected_ty = None;
          actual_ty = None;
          cursor_position = index;
          num_nodes = Syntax.zsize tree;
        }
    | Arrow_L (t, _) | Prod_L (t, _) | List_L t ->
        get_cursor_info_type ~current_term:t
          ~parent_term:(Some (ZTNode current_term)) ~index:(index + 1)
    | Arrow_R (t1, t2) | Prod_R (t1, t2) ->
        get_cursor_info_type ~current_term:t2
          ~parent_term:(Some (ZTNode current_term))
          ~index:(index + Type.size t1 + 1)
  in
  let rec get_cursor_info_pattern ~(current_term : Pattern.z_t)
      ~(parent_term : Syntax.z_t option) ~(exp_ty : Type.p_t) ~(index : int) =
    match current_term.node with
    | Cursor p ->
        let p : Pattern.t = { (Pattern.unzip current_term) with node = p } in
        {
          current_term = PNode p;
          parent_term;
          vars_in_scope = [];
          args_in_scope = [];
          typ_ctx = [];
          expected_ty = Some exp_ty;
          actual_ty = Some (pattern_type (Pattern.strip p));
          cursor_position = index;
          num_nodes = Syntax.zsize tree;
        }
    | PCons_L (p, _) ->
        let exp_ty =
          match exp_ty with
          | List t -> t
          | Hole -> Hole
          | _ -> raise (TypeError "Expected list type")
        in
        get_cursor_info_pattern ~current_term:p
          ~parent_term:(Some (ZPNode current_term)) ~exp_ty ~index:(index + 1)
    | PCons_R (p1, p2) ->
        let exp_ty =
          match exp_ty with
          | List t -> exp_ty
          | Hole -> List Hole
          | _ -> raise (TypeError "Expected list type")
        in
        get_cursor_info_pattern ~current_term:p2
          ~parent_term:(Some (ZPNode current_term)) ~exp_ty
          ~index:(index + Pattern.size p1 + 1)
  in
  let rec get_cursor_info_expr ~(current_term : Expr.z_t)
      ~(parent_term : Expr.z_t option) ~(vars : (Var.t * int) list)
      ~(args : (Var.t * int * int) list) ~(typ_ctx : Context.t)
      ~(exp_ty : Type.p_t) ~(index : int) =
    match current_term.node with
    | Cursor e -> (
        let e : Expr.t = { (Expr.unzip current_term) with node = e } in
        match synthesis typ_ctx e with
        | Some t ->
            let parent_term =
              match parent_term with
              | Some e -> Some (Syntax.ZENode e)
              | None -> None
            in
            {
              current_term = Syntax.ENode e;
              parent_term;
              vars_in_scope = vars;
              args_in_scope = args;
              typ_ctx;
              expected_ty = Some exp_ty;
              actual_ty = Some t;
              cursor_position = index;
              num_nodes = Syntax.zsize tree;
            }
        | None ->
            raise
              (TypeError
                 ("Incorrect type: "
                 ^ Core.Sexp.to_string (Syntax.sexp_of_z_t tree))))
    | EUnOp_L (OpNeg, e) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Int ~index:(index + 1)
    | EBinOp_L
        ( e,
          ( OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe
          | OpEq | OpNe ),
          _ ) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Int ~index:(index + 1)
    | EBinOp_R
        ( e1,
          ( OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe
          | OpEq | OpNe ),
          e2 ) ->
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Int
          ~index:(index + Expr.size e1 + 1)
    | EBinOp_L (e, (OpAnd | OpOr), _) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Bool ~index:(index + 1)
    | EBinOp_R (e1, (OpAnd | OpOr), e2) ->
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Bool
          ~index:(index + Expr.size e1 + 1)
    | EBinOp_L (e1, OpCons, e2) ->
        let exp_ty =
          match synthesis typ_ctx e2 with
          | Some Type.Hole -> Type.Hole
          | Some (Type.List t) -> t
          | _ -> raise (TypeError "Expected a list type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EBinOp_R (e1, OpCons, e2) ->
        let exp_ty =
          match synthesis typ_ctx e1 with
          | Some t -> Type.List t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EBinOp_L (e1, OpAp, e2) ->
        let exp_ty =
          match synthesis typ_ctx e2 with
          | Some t -> Type.Arrow (t, exp_ty)
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EBinOp_R (e1, OpAp, e2) ->
        let exp_ty =
          match synthesis typ_ctx e1 with
          | Some Type.Hole -> Type.Hole
          | Some (Type.Arrow (tin, tout)) -> tin
          | _ -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    (* Prevent type incorrect changes of edef *)
    | ELet_L (_, edef, ebody) ->
        get_cursor_info_expr ~current_term:edef ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Hole ~index:(index + 2)
    | ELet_R (x, edef, ebody) ->
        let x_type =
          match synthesis typ_ctx edef with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:ebody
          ~parent_term:(Some current_term)
          ~vars:((x, index + 1) :: vars)
          ~args
          ~typ_ctx:(Context.extend typ_ctx (x, x_type))
          ~exp_ty
          ~index:(index + Expr.size edef + 2)
    | EIf_L (econd, _, _) ->
        get_cursor_info_expr ~current_term:econd
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx
          ~exp_ty:Type.Bool ~index:(index + 1)
    | EIf_C (econd, ethen, eelse) ->
        let exp_ty =
          let t_else =
            match synthesis typ_ctx eelse with
            | Some t -> t
            | None -> raise (TypeError "Type cannot be inferred")
          in
          match Typing.get_common_type exp_ty t_else with
          | Some t -> t
          | None ->
              raise
                (TypeError
                   ("Conflicting types between expected type "
                  ^ TypeConv.to_string exp_ty ^ "and type of else branch "
                  ^ TypeConv.to_string t_else))
        in
        get_cursor_info_expr ~current_term:ethen
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size econd + 1)
    | EIf_R (econd, ethen, eelse) ->
        let exp_ty =
          let t_then =
            match synthesis typ_ctx ethen with
            | Some t -> t
            | None -> raise (TypeError "Type cannot be inferred")
          in
          match Typing.get_common_type exp_ty t_then with
          | Some t -> t
          | None ->
              raise
                (TypeError
                   ("Conflicting types between expected type "
                  ^ TypeConv.to_string exp_ty ^ "and type of then branch "
                  ^ TypeConv.to_string t_then))
        in
        get_cursor_info_expr ~current_term:eelse
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size econd + Expr.size ethen + 1)
    | EFun_L (_, t, _) | EFix_L (_, t, _) ->
        get_cursor_info_type ~current_term:t
          ~parent_term:(Some (Syntax.ZENode current_term)) ~index:(index + 1)
    | EFun_R (x, t, e) | EFix_R (x, t, e) ->
        let exp_ty =
          match exp_ty with Type.Arrow (tin, tout) -> tout | _ -> Type.Hole
          (* | Type.Hole -> Type.Hole
             | _ -> raise (TypeError "Expected a function type") *)
        in
        let arg =
          match parent_term with
          (* If it's one of multiple arguments, fun_index does not change, arg_index + 1 *)
          | Some { node = EFun_R _; _ } ->
              let _, fun_index, arg_index = List.nth args 0 in
              (x, fun_index, arg_index + 1)
          | _ -> (
              match args with
              | [] -> (x, 0, 0)
              | (_, fun_index, _) :: tl -> (x, fun_index + 1, 0))
        in
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args:(arg :: args)
          ~typ_ctx:(Context.extend typ_ctx (x, Type.strip t))
          ~exp_ty
          ~index:(index + Type.size t + 2)
    | EPair_L (e1, e2) ->
        let exp_ty =
          match exp_ty with
          | Type.Prod (t1, t2) -> t1
          | Type.Hole -> Type.Hole
          | _ -> raise (TypeError "Expected a product type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EPair_R (e1, e2) ->
        let exp_ty =
          match exp_ty with
          | Type.Prod (t1, t2) -> t2
          | Type.Hole -> Type.Hole
          | _ -> raise (TypeError "Expected a product type")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EFilter_L (e1, e2) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx e2) with
          | Hole, Some Hole -> Type.Arrow (Hole, Bool)
          | Hole, Some (Type.List foundtype) -> Type.Arrow (foundtype, Bool)
          | List exptype, Some Hole -> Type.Arrow (exptype, Bool)
          | List exptype, Some (Type.List foundtype) ->
              if Type.consistent exptype foundtype
              then Type.Arrow (foundtype, Type.Bool)
              else raise (TypeError "List types do not match")
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EFilter_R (e1, e2) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx e1) with
          | Hole, Some Hole -> Type.List Hole
          | Hole, Some (Arrow (t, (Bool | Hole))) | List t, Some Hole ->
              Type.List t
          | List listtype, Some (Arrow (functype, (Bool | Hole))) ->
              if Type.consistent listtype functype
              then exp_ty
              else raise (TypeError "List types do not match")
          | _, Some t ->
              raise
                (TypeError
                   ("Expected a List type but got " ^ TypeConv.to_string exp_ty
                  ^ " and " ^ TypeConv.to_string t))
          | _, None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EFold_L (efunc, eaccum, elist) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx eaccum, synthesis typ_ctx elist) with
          | Hole, Some Hole, Some Hole ->
              Type.Arrow (Hole, Type.Arrow (Hole, Hole))
          | Hole, Some Hole, Some (Type.List listtype) ->
              Type.Arrow (Hole, Type.Arrow (listtype, Hole))
          | Hole, Some acctype, Some (Type.List listtype) ->
              Type.Arrow (acctype, Type.Arrow (listtype, acctype))
          | exptype, Some acctype, Some Hole -> (
              match get_common_type exptype acctype with
              | Some t -> Type.Arrow (t, Type.Arrow (Hole, t))
              | None ->
                  raise
                    (TypeError
                       "Expected type does not match with accumulator type"))
          | exptype, Some acctype, Some (Type.List listtype) -> (
              match get_common_type exptype acctype with
              | Some t -> Type.Arrow (t, Type.Arrow (listtype, t))
              | None ->
                  raise
                    (TypeError
                       "Expected type does not match with accumulator type"))
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:efunc
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + 1)
    | EFold_C (efunc, eaccum, elist) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx efunc, synthesis typ_ctx elist) with
          | Hole, Some Hole, Some (Hole | List _) -> Type.Hole
          | Hole, Some (Arrow (intype, Hole)), Some (Hole | List _) -> intype
          | Hole, Some (Type.Arrow (intype, Type.Arrow (_, outtype))), Some Hole
            ->
              if Type.consistent intype outtype
              then intype
              else raise (TypeError "inconsistent types in fold")
          | ( Hole,
              Some (Type.Arrow (intype, Type.Arrow (listfunctype, outtype))),
              Some (Type.List listtype) ) ->
              if Type.consistent intype outtype
                 && Type.consistent listfunctype listtype
              then intype
              else raise (TypeError "inconsistent types in fold")
          | exptype, Some Hole, Some (Hole | List _) -> exptype
          | exptype, Some (Arrow (intype, Hole)), Some (Hole | List _) -> (
              match get_common_type exptype intype with
              | Some t -> t
              | None -> raise (TypeError "inconsistent types in fold"))
          | ( exptype,
              Some (Type.Arrow (intype, Type.Arrow (listfunctype, outtype))),
              Some Hole ) -> (
              match get_common_type exptype intype with
              | Some t -> (
                  match get_common_type t outtype with
                  | Some t -> t
                  | None -> raise (TypeError "inconsistent types in fold"))
              | None -> raise (TypeError "inconsistent types in fold"))
          | ( exptype,
              Some (Type.Arrow (intype, Type.Arrow (listfunctype, outtype))),
              Some (Type.List listtype) ) -> (
              match get_common_type exptype intype with
              | Some t -> (
                  match get_common_type t outtype with
                  | Some t ->
                      if Type.consistent listtype listfunctype
                      then t
                      else raise (TypeError "inconsistent types in fold")
                  | None -> raise (TypeError "inconsistent types in fold"))
              | None -> raise (TypeError "inconsistent types in fold"))
          | a, Some b, Some c ->
              raise
                (TypeError
                   ("inconsistent types in fold " ^ TypeConv.to_string a
                  ^ TypeConv.to_string b ^ TypeConv.to_string c))
          | _ -> raise (TypeError "inconsistent types in fold")
        in
        get_cursor_info_expr ~current_term:eaccum
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size efunc + 1)
    | EFold_R (efunc, eaccum, elist) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx efunc, synthesis typ_ctx eaccum) with
          | Hole, Some Hole, Some _ | Hole, Some (Arrow (_, Hole)), Some _ ->
              Type.List Hole
          | ( Hole,
              Some (Type.Arrow (intype, Type.Arrow (accumfunctype, outtype))),
              Some accumtype ) ->
              if Type.consistent intype outtype
              then Type.List accumfunctype
              else raise (TypeError "inconsistent types in fold")
          | exptype, Some (Hole | Arrow (_, Hole)), Some _ -> Type.List Hole
          | ( exptype,
              Some (Type.Arrow (intype, Type.Arrow (accumfunctype, outtype))),
              Some accumtype ) ->
              if Type.consistent intype outtype
                 (* check all three because consistency isnt necessarily transitive*)
                 && Type.consistent exptype intype
                 && Type.consistent exptype outtype
              then Type.List accumfunctype
              else raise (TypeError "inconsistent types in fold ")
          | _ -> raise (TypeError "inconsistent types in fold")
        in
        get_cursor_info_expr ~current_term:elist
          ~parent_term:(Some current_term) ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size efunc + Expr.size eaccum + 1)
    | EMap_L (e1, e2) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx e2) with
          | Hole, Some Hole -> Type.Arrow (Hole, Hole)
          | Hole, Some (Type.List foundtype) -> Type.Arrow (foundtype, Hole)
          | List exptype, Some Hole -> Type.Arrow (Hole, exptype)
          | List exptype, Some (Type.List foundtype) ->
              Type.Arrow (foundtype, exptype)
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EMap_R (e1, e2) ->
        let exp_ty =
          match (exp_ty, synthesis typ_ctx e1) with
          | Hole, Some Hole -> Type.List Hole
          | Hole, Some (Arrow (functype, _)) -> Type.List functype
          | List exptype, Some Hole -> Type.List Hole
          | List exptype, Some (Arrow (intype, outtype)) ->
              if Type.consistent exptype outtype
              then Type.List intype
              else raise (TypeError "Function type does not match list")
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EListEq_L (e1, e2) ->
        let exp_ty =
          match synthesis typ_ctx e2 with
          | Some (Type.List t) -> Type.List t
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty ~index:(index + 1)
    | EListEq_R (e1, e2) ->
        let exp_ty =
          match synthesis typ_ctx e1 with
          | Some (Type.List t) -> Type.List t
          | _ -> raise (TypeError "Expected a List type")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EMatch_L (e, _, _) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Hole ~index:(index + 1)
    | EMatch_P1 (e, (p1, _), (p2, _)) ->
        let t_scrut =
          match synthesis typ_ctx e with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        let t_pat = pattern_type (Pattern.strip p2) in
        let exp_ty =
          match Typing.get_common_type t_scrut t_pat with
          | Some t -> t
          | None ->
              raise
                (TypeError
                   "Conflicting types between expected type and type of pattern")
        in
        get_cursor_info_pattern ~current_term:p1
          ~parent_term:(Some (Syntax.ZENode current_term)) ~exp_ty
          ~index:(index + Expr.size e + 1)
    | EMatch_E1 (e, (p1, e1), (p2, e2)) ->
        let exp_ty =
          match pattern_common_type p1 p2 with
          | Some t -> (
              let t2 =
                match get_rule_type p2 e2 typ_ctx t with
                | Some t -> t
                | None -> raise (TypeError "Invalid rule type")
              in
              match Typing.get_common_type exp_ty t2 with
              | Some t -> t
              | None ->
                  raise
                    (TypeError
                       "Conflicting types between expected type and type of \
                        other rules"))
          | None -> raise (TypeError "Conflicting types between patterns")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e + Pattern.size p1 + 1)
    | EMatch_P2 (e, (p1, e1), (p2, _)) ->
        let t_scrut =
          match synthesis typ_ctx e with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        let t_pat = pattern_type (Pattern.strip p1) in
        let exp_ty =
          match Typing.get_common_type t_scrut t_pat with
          | Some t -> t
          | None ->
              raise
                (TypeError
                   "Conflicting types between expected type and type of pattern")
        in
        get_cursor_info_pattern ~current_term:p2
          ~parent_term:(Some (Syntax.ZENode current_term)) ~exp_ty
          ~index:(index + Expr.size e + Pattern.size p1 + Expr.size e1 + 1)
    | EMatch_E2 (e, (p1, e1), (p2, e2)) ->
        let exp_ty =
          match pattern_common_type p1 p2 with
          | Some t -> (
              let t1 =
                match get_rule_type p1 e1 typ_ctx t with
                | Some t -> t
                | None -> raise (TypeError "Invalid rule type")
              in
              match Typing.get_common_type exp_ty t1 with
              | Some t -> t
              | None ->
                  raise
                    (TypeError
                       "Conflicting types between expected type and type of \
                        other rules"))
          | None -> raise (TypeError "Conflicting types between patterns")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty
          ~index:
            (index + Expr.size e + Pattern.size p1 + Expr.size e1
           + Pattern.size p2 + 1)
    | EAssert_L e ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~args ~typ_ctx ~exp_ty:Type.Bool ~index:(index + 1)
  in
  match tree with
  | ZENode e ->
      get_cursor_info_expr ~current_term:e ~parent_term:None ~vars:[] ~args:[]
        ~typ_ctx:[] ~exp_ty:Type.Hole ~index:0
  | ZTNode t -> get_cursor_info_type ~current_term:t ~parent_term:None ~index:0
  | ZPNode p ->
      get_cursor_info_pattern ~current_term:p ~parent_term:None
        ~exp_ty:Type.Hole ~index:0

let%test_module "Test get_cursor_info" =
  (module struct
    let equal i i' =
      Syntax.equal i.current_term i'.current_term
      && (match (i.parent_term, i'.parent_term) with
         | Some _, Some _ | None, None -> true
         | _ -> false)
      && i.vars_in_scope = i'.vars_in_scope
      && i.args_in_scope = i'.args_in_scope
      && i.typ_ctx = i'.typ_ctx
      && i.expected_ty = i'.expected_ty
      && i.actual_ty = i'.actual_ty
      && i.cursor_position = i'.cursor_position
      && i.num_nodes = i'.num_nodes

    let check e i = equal (get_cursor_info (ZENode e)) i
    let e : Expr.z_t = { id = -1; node = Expr.Cursor EHole; starter = false }
    (* ^<HOLE> *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node Expr.EHole);
        parent_term = None;
        vars_in_scope = [];
        args_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Hole;
        cursor_position = 0;
        num_nodes = 1;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      { id = -1; node = Expr.Cursor (EConst (Int 1)); starter = false }
    (* ^1 *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node (EConst (Int 1)));
        parent_term = None;
        vars_in_scope = [];
        args_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Int;
        cursor_position = 0;
        num_nodes = 1;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.EUnOp_L
            ( OpNeg,
              {
                id = -1;
                node = Expr.Cursor (EConst (Bool true));
                starter = false;
              } );
        starter = false;
      }
    (* -(^true) *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node (Expr.EConst (Bool true)));
        parent_term = Some (ZENode e);
        vars_in_scope = [];
        args_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Int;
        actual_ty = Some Type.Bool;
        cursor_position = 1;
        num_nodes = 2;
      }

    let%test _ = check e i

    (* Remove checks on current & parent terms *)
    let equal i i' =
      i.vars_in_scope = i'.vars_in_scope
      && i.args_in_scope = i'.args_in_scope
      && i.typ_ctx = i'.typ_ctx
      && i.expected_ty = i'.expected_ty
      && i.actual_ty = i'.actual_ty
      && i.cursor_position = i'.cursor_position

    let check e i = equal (get_cursor_info (ZENode e)) i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.ELet_R
            ( 0,
              Expr.make_dummy_node (EConst (Int 1)),
              { id = -1; node = Expr.Cursor (EVar 0); starter = false } );
        starter = false;
      }
    (* let x0 = 1 in ^x0 *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [ (0, 1) ];
        args_in_scope = [];
        typ_ctx = [ (0, Type.Int) ];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Int;
        cursor_position = 3;
        num_nodes = 4;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.ELet_R
            ( 0,
              Expr.make_dummy_node (EConst (Int 1)),
              {
                id = -1;
                node =
                  Expr.ELet_R
                    ( 1,
                      Expr.make_dummy_node (EConst (Bool false)),
                      {
                        id = -1;
                        node =
                          Expr.EBinOp_L
                            ( Expr.make_z_node (Expr.Cursor (EVar 1)),
                              OpAp,
                              Expr.make_dummy_node (EConst (Int 2)) );
                        starter = false;
                      } );
                starter = false;
              } );
        starter = false;
      }
    (*
       let x0 = 1 in
         let x1 = false in
           x1 ^2
    *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [ (1, 4); (0, 1) ];
        args_in_scope = [];
        typ_ctx = [ (1, Type.Bool); (0, Type.Int) ];
        expected_ty = Some (Type.Arrow (Type.Int, Type.Hole));
        actual_ty = Some Type.Bool;
        cursor_position = 7;
        num_nodes = 9;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.EIf_L
            ( Expr.make_z_node (Cursor EHole),
              Expr.make_dummy_node EHole,
              Expr.make_dummy_node EHole );
        starter = false;
      }
    (* if ^<HOLE> then <HOLE> else <HOLE> *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [];
        args_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Bool;
        actual_ty = Some Type.Hole;
        cursor_position = 1;
        num_nodes = 4;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.EIf_C
            ( Expr.make_dummy_node EHole,
              Expr.make_z_node (Cursor (EConst (Bool true))),
              Expr.make_dummy_node (EConst (Int 1)) );
        starter = false;
      }
    (* if <HOLE> then ^true else 1 *)

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [];
        args_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Int;
        actual_ty = Some Type.Bool;
        cursor_position = 2;
        num_nodes = 4;
      }

    let%test _ = check e i
  end)

let max_num_nodes = 200

let ints =
  [
    Action.Construct (Const (Int (-2)));
    Construct (Const (Int (-1)));
    Construct (Const (Int 0));
    Construct (Const (Int 1));
    Construct (Const (Int 2));
  ]

let bools =
  [ Action.Construct (Const (Bool true)); Construct (Const (Bool false)) ]

let arith =
  [
    Action.Construct (BinOp_L OpPlus);
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
    Action.Construct (BinOp_L OpLt);
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

let logic =
  [
    Action.Construct (BinOp_L OpAnd);
    Construct (BinOp_L OpOr);
    Construct (BinOp_R OpAnd);
    Construct (BinOp_R OpOr);
  ]

let pat_int =
  [
    Action.Construct (PatConst (Int (-2)));
    Construct (PatConst (Int (-1)));
    Construct (PatConst (Int 0));
    Construct (PatConst (Int 1));
    Construct (PatConst (Int 2));
  ]

let pat_bool =
  [ Action.Construct (PatConst (Bool true)); Construct (PatConst (Bool false)) ]

(* Given the info at the cursor, return a list of possible actions *)
let cursor_info_to_actions (info : t) : Action.t list =
  let open Action in
  let handle_move _ =
    let handle_parent _ =
      match info.parent_term with
      | Some (ZENode tree) -> if tree.starter then [] else [ Move Parent ]
      | Some (ZTNode tree) -> if tree.starter then [] else [ Move Parent ]
      | Some (ZPNode tree) -> if tree.starter then [] else [ Move Parent ]
      | None -> []
    in
    let handle_child _ =
      match info.current_term with
      | ENode e -> (
          match e.node with
          | EVar _ | EConst _ | EHole -> []
          | EUnOp _ | EAssert _ -> [ Move (Child 0) ]
          | EBinOp _ | EFun _ | EFix _ | EPair _ | EMap _ | EFilter _
          | EListEq _ ->
              [ Move (Child 0); Move (Child 1) ]
          | EFold _ | EIf _ ->
              [ Move (Child 0); Move (Child 1); Move (Child 2) ]
          | ELet (_, edef, _) -> (
              match synthesis info.typ_ctx edef with
              | Some Type.Hole -> [ Move (Child 0) ]
              | Some _ -> [ Move (Child 0); Move (Child 1) ]
              | None -> raise (TypeError "Type cannot be inferred"))
          | EMatch _ ->
              [
                Move (Child 0);
                Move (Child 1);
                Move (Child 2);
                Move (Child 3);
                Move (Child 4);
              ])
      | TNode t -> (
          match t.node with
          | TInt | TBool | THole | TUnit -> []
          | TList _ -> [ Move (Child 0) ]
          | TArrow _ | TProd _ -> [ Move (Child 0); Move (Child 1) ])
      | PNode p -> (
          match p.node with
          | PVar _ | PConst _ | PWild -> []
          | PCons _ -> [ Move (Child 0); Move (Child 1) ])
    in
    handle_parent () @ handle_child ()
  in
  let handle_expr _ =
    let exp_ty =
      match info.expected_ty with
      | Some t -> t
      | None -> raise (TypeError "Invalid expected type")
    in
    let actual_ty =
      match info.actual_ty with
      | Some t -> t
      | None -> raise (TypeError "Invalid actual type")
    in
    let construct_atom _ =
      (* TODO: How to construct vars *)
      match exp_ty with
      | Type.Int -> Construct Hole :: ints
      | Type.Bool -> Construct Hole :: bools
      | Type.List _ -> [ Construct (Const Nil); Construct Hole ]
      | Type.Hole -> [ Construct (Const Nil); Construct Hole ] @ ints @ bools
      | _ -> []
    in
    let construct_unop _ =
      match exp_ty with
      | Type.Int | Type.Hole -> (
          match actual_ty with
          | Type.Int | Type.Hole -> [ Construct (UnOp OpNeg) ]
          | _ -> [])
      | _ -> []
    in
    let construct_binop _ =
      let construct_arith_comp_logic _ =
        match exp_ty with
        | Type.Int -> (
            match actual_ty with Type.Int | Type.Hole -> arith | _ -> [])
        | Type.Bool -> (
            match actual_ty with
            | Type.Int -> comp
            | Type.Bool -> logic
            | Type.Hole -> comp @ logic
            | _ -> [])
        | Type.Hole -> (
            match actual_ty with
            | Type.Int -> arith @ comp
            | Type.Bool -> logic
            | Type.Hole -> arith @ comp @ logic
            | _ -> [])
        | _ -> []
      in
      let construct_ap _ =
        match actual_ty with
        | Type.Arrow (tin, tout) ->
            if Type.consistent exp_ty tout
            then [ Construct (BinOp_L OpAp); Construct (BinOp_R OpAp) ]
            else [ Construct (BinOp_R OpAp) ]
        | Type.Hole -> [ Construct (BinOp_L OpAp); Construct (BinOp_R OpAp) ]
        | _ -> [ Construct (BinOp_R OpAp) ]
      in
      let construct_cons _ =
        match exp_ty with
        | Type.List t ->
            let l_consistent = Type.consistent actual_ty t in
            let r_consistent = Type.consistent actual_ty exp_ty in
            if l_consistent && r_consistent
            then [ Construct (BinOp_L OpCons); Construct (BinOp_R OpCons) ]
            else if l_consistent
            then [ Construct (BinOp_L OpCons) ]
            else if r_consistent
            then [ Construct (BinOp_R OpCons) ]
            else []
        | Type.Hole -> (
            match actual_ty with
            | Type.List _ | Type.Hole ->
                [ Construct (BinOp_L OpCons); Construct (BinOp_R OpCons) ]
            | _ -> [ Construct (BinOp_L OpCons) ])
        | _ -> []
      in
      construct_arith_comp_logic () @ construct_ap () @ construct_cons ()
    in
    let construct_let _ =
      if !Var.num_vars < Var.max_num_vars then [ Construct Let_L ] else []
    in
    let construct_if _ =
      let cond_consistent = Type.consistent actual_ty Type.Bool in
      let body_consistent = Type.consistent actual_ty exp_ty in
      if cond_consistent && body_consistent
      then [ Construct If_L; Construct If_C; Construct If_R ]
      else if cond_consistent
      then [ Construct If_L ]
      else if body_consistent
      then [ Construct If_C; Construct If_R ]
      else []
    in
    let construct_fun_fix _ =
      (* TODO: Allow changing type annotations? *)
      if Type.consistent exp_ty (Type.Arrow (Type.Hole, actual_ty))
         && !Var.num_vars < Var.max_num_vars
      then [ (* Construct Fun; Construct Fix *) ]
      else []
    in
    let construct_pair _ =
      let l_consistent =
        Type.consistent exp_ty (Type.Prod (actual_ty, Type.Hole))
      in
      let r_consistent =
        Type.consistent exp_ty (Type.Prod (Type.Hole, actual_ty))
      in
      if l_consistent && r_consistent
      then [ Construct Pair_L; Construct Pair_R ]
      else if l_consistent
      then [ Construct Pair_L ]
      else if r_consistent
      then [ Construct Pair_R ]
      else []
    in
    let construct_map _ =
      match actual_ty with
      | Type.Arrow (tin, tout) ->
          if Type.consistent exp_ty (Type.List tout)
          then [ Construct Map_L ]
          else []
      | Type.List _ -> [ Construct Map_R ]
      | Type.Hole -> [ Construct Map_L; Construct Map_L ]
      | _ -> []
    in
    let construct_filter _ =
      match actual_ty with
      | Type.Arrow (tin, tout) ->
          if Type.consistent exp_ty (Type.List tin)
          then [ Construct Filter_L ]
          else []
      | Type.List _ -> [ Construct Filter_R ]
      | Type.Hole -> [ Construct Filter_L; Construct Filter_L ]
      | _ -> []
    in
    let construct_fold _ =
      match actual_ty with
      | Type.Arrow (tin, Type.Arrow (tlist, tout)) ->
          if Type.consistent exp_ty tin && Type.consistent exp_ty tout
          then [ Construct Fold_L; Construct Fold_C ]
          else [ Construct Fold_C ]
      | Type.List _ -> [ Construct Fold_C; Construct Fold_R ]
      | Type.Hole -> [ Construct Fold_L; Construct Fold_C; Construct Fold_R ]
      | _ -> [ Construct Fold_C ]
    in
    let construct_var _ =
      let rec construct_var_aux n lst =
        match lst with
        | [] -> []
        | (var, _) :: tl -> (
            match Context.lookup info.typ_ctx var with
            | Some t ->
                if Type.consistent t exp_ty
                then Construct (Var n) :: construct_var_aux (n + 1) tl
                else construct_var_aux (n + 1) tl
            | None -> raise (Failure "Not in typing context"))
      in
      let rec construct_arg_aux n lst =
        match lst with
        | [] -> []
        | (var, _, _) :: tl -> (
            match Context.lookup info.typ_ctx var with
            | Some t ->
                if Type.consistent t exp_ty
                then Construct (Arg n) :: construct_arg_aux (n + 1) tl
                else construct_arg_aux (n + 1) tl
            | None -> raise (Failure "Not in typing context"))
      in
      construct_var_aux 0 info.vars_in_scope
      @ construct_arg_aux 0 info.args_in_scope
    in
    let construct_match _ =
      [ Construct Match_L; Construct Match_E1; Construct Match_E2 ]
    in
    let handle_unwrap _ =
      let rec check_var (e : Expr.t) (x : Var.t) : bool =
        match e.node with
        | EVar v -> Var.equal x v
        | EHole | EConst _ -> false
        | EUnOp (_, e) | EFun (_, _, e) | EFix (_, _, e) | EAssert e ->
            check_var e x
        | EBinOp (e1, _, e2)
        | EPair (e1, e2)
        | ELet (_, e1, e2)
        | EMap (e1, e2)
        | EFilter (e1, e2)
        | EListEq (e1, e2) ->
            check_var e1 x || check_var e2 x
        | EFold (e1, e2, e3) | EIf (e1, e2, e3) ->
            check_var e1 x || check_var e2 x || check_var e3 x
        | EMatch (e, (p1, e1), (p2, e2)) ->
            check_var e x || check_var e1 x || check_var e2 x
      in
      match info.current_term with
      | ENode e -> (
          match e.node with
          | EHole | EConst _ | EVar _ | EAssert _ | EListEq _ -> []
          | EUnOp _ -> [ Unwrap 0 ]
          | EFilter _ -> [ Unwrap 1 ]
          | EMap (func, listarg) -> (
              match
                (synthesis info.typ_ctx func, synthesis info.typ_ctx listarg)
              with
              | Some (Arrow (intype, outtype)), Some (List listtype) ->
                  if Type.consistent intype outtype
                     && Type.consistent outtype listtype
                  then [ Unwrap 1 ]
                  else []
              | _ -> [])
          | EFold (e1, e2, e3) ->
              if analysis info.typ_ctx e1 exp_ty
              then
                if analysis info.typ_ctx e3 exp_ty
                then [ Unwrap 0; Unwrap 1; Unwrap 3 ]
                else [ Unwrap 0; Unwrap 1 ]
              else [ Unwrap 1 ]
          | EBinOp (e1, _, e2) | EPair (e1, e2) ->
              let t1 =
                match Typing.synthesis info.typ_ctx e1 with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let t2 =
                match Typing.synthesis info.typ_ctx e2 with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let l_consistent = Type.consistent t1 exp_ty in
              let r_consistent = Type.consistent t2 exp_ty in
              if l_consistent && r_consistent
              then [ Unwrap 0; Unwrap 1 ]
              else if l_consistent
              then [ Unwrap 0 ]
              else if r_consistent
              then [ Unwrap 1 ]
              else []
          | ELet (x, edef, ebody) ->
              (* Check if there are uses of the variable *)
              let t_def =
                match Typing.synthesis info.typ_ctx edef with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let t_body =
                match
                  Typing.synthesis
                    (Context.extend info.typ_ctx (x, t_def))
                    ebody
                with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let def_consistent = Type.consistent exp_ty t_def in
              let body_consistent = Type.consistent exp_ty t_body in
              let check_var = check_var ebody x in
              if not check_var
              then
                if def_consistent && body_consistent
                then [ Unwrap 0; Unwrap 1 ]
                else if def_consistent
                then [ Unwrap 0 ]
                else if body_consistent
                then [ Unwrap 1 ]
                else []
              else if def_consistent
              then [ Unwrap 0 ]
              else []
          | EIf (econd, ethen, eelse) ->
              let t_cond =
                match Typing.synthesis info.typ_ctx econd with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let t_body =
                match Typing.synthesis info.typ_ctx ethen with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let cond_consistent = Type.consistent exp_ty t_cond in
              let body_consistent = Type.consistent exp_ty t_body in
              if cond_consistent && body_consistent
              then [ Unwrap 0; Unwrap 1; Unwrap 2 ]
              else if cond_consistent
              then [ Unwrap 0 ]
              else if body_consistent
              then [ Unwrap 1; Unwrap 2 ]
              else []
          | EFun (x, ty, e) | EFix (x, ty, e) ->
              let check_var = check_var e x in
              let ty = Type.strip ty in
              let t =
                match
                  Typing.synthesis (Context.extend info.typ_ctx (x, ty)) e
                with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              if Type.consistent exp_ty t && not check_var
              then [ Unwrap 1 ]
              else []
          | EMatch (e, (p1, e1), (p2, e2)) ->
              let t_scrut =
                match Typing.synthesis info.typ_ctx e with
                | Some t -> t
                | None -> raise (TypeError "Invalid type")
              in
              let t =
                match pattern_common_type p1 p2 with
                | Some t -> t
                | None -> raise (TypeError "Inconsistent pattern types")
              in
              let t1 =
                match get_rule_type p1 e1 info.typ_ctx t with
                | Some t -> t
                | None -> raise (TypeError "Invalid rule type")
              in
              let t2 =
                match get_rule_type p2 e2 info.typ_ctx t with
                | Some t -> t
                | None -> raise (TypeError "Invalid rule type")
              in
              let rec get_vars_pattern (p : Pattern.t) =
                match p.node with
                | PVar x -> [ x ]
                | PCons (p1, p2) -> get_vars_pattern p1 @ get_vars_pattern p2
                | PConst _ | PWild -> []
              in
              let scrut_consistent = Type.consistent exp_ty t_scrut in
              let check_var1 =
                List.exists (check_var e1) (get_vars_pattern p1)
              in
              let t1_consistent = Type.consistent exp_ty t1 && not check_var1 in
              let check_var2 =
                List.exists (check_var e2) (get_vars_pattern p2)
              in
              let t2_consistent = Type.consistent exp_ty t2 && not check_var2 in
              if scrut_consistent && t1_consistent && t2_consistent
              then [ Unwrap 0; Unwrap 1; Unwrap 2 ]
              else if scrut_consistent && t1_consistent
              then [ Unwrap 0; Unwrap 1 ]
              else if scrut_consistent && t2_consistent
              then [ Unwrap 0; Unwrap 2 ]
              else if t1_consistent && t2_consistent
              then [ Unwrap 1; Unwrap 2 ]
              else if scrut_consistent
              then [ Unwrap 0 ]
              else if t1_consistent
              then [ Unwrap 1 ]
              else if t2_consistent
              then [ Unwrap 2 ]
              else [])
      | TNode _ -> []
      | PNode _ -> []
    in
    let remaining_nodes = max_num_nodes - info.num_nodes in
    let actions =
      if remaining_nodes = 0
      then [ construct_atom (); construct_var () ]
      else if remaining_nodes = 1
      then [ construct_atom (); construct_var (); construct_unop () ]
      else if remaining_nodes = 2
      then
        [
          construct_atom ();
          construct_var ();
          construct_unop ();
          construct_binop ();
          construct_pair ();
          construct_pair ();
          construct_filter ();
        ]
      else if remaining_nodes = 3
      then
        [
          construct_atom ();
          construct_unop ();
          construct_binop ();
          construct_pair ();
          construct_filter ();
          construct_let ();
          construct_if ();
          construct_fold ();
          construct_fun_fix ();
          construct_pair ();
          construct_var ();
          handle_unwrap ();
        ]
      else if remaining_nodes = 3
      then
        [
          construct_atom ();
          construct_unop ();
          construct_binop ();
          construct_let ();
          construct_if ();
          construct_fold ();
          construct_fun_fix ();
          construct_pair ();
          construct_var ();
          handle_unwrap ();
        ]
      else
        [
          construct_atom ();
          construct_unop ();
          construct_binop ();
          construct_let ();
          construct_pair ();
          construct_filter ();
          construct_if ();
          construct_fold ();
          construct_fun_fix ();
          construct_pair ();
          construct_filter ();
          construct_map ();
          construct_var ();
          construct_match ();
          handle_unwrap ();
        ]
    in
    List.concat actions
  in
  let handle_type _ = [] in
  let handle_pattern _ =
    match info.expected_ty with
    | Some Int -> [ Construct PatVar; Construct PatWild ] @ pat_int
    | Some Bool -> [ Construct PatVar; Construct PatWild ] @ pat_bool
    | Some (List t) ->
        let actual_ty =
          match info.actual_ty with
          | Some t -> t
          | None -> raise (TypeError "Invalid type")
        in
        let l_consistent = Type.consistent actual_ty t in
        let r_consistent = Type.consistent actual_ty (List t) in
        if l_consistent && r_consistent
        then
          [
            Construct PatVar;
            Construct PatWild;
            Construct (PatConst Nil);
            Construct PatCons_L;
            Construct PatCons_R;
          ]
        else if l_consistent
        then
          [
            Construct PatVar;
            Construct PatWild;
            Construct (PatConst Nil);
            Construct PatCons_L;
          ]
        else if r_consistent
        then
          [
            Construct PatVar;
            Construct PatWild;
            Construct (PatConst Nil);
            Construct PatCons_R;
          ]
        else [ Construct PatVar; Construct PatWild; Construct (PatConst Nil) ]
    | Some Hole ->
        [
          Construct PatVar;
          Construct (PatConst Nil);
          Construct PatWild;
          Construct PatCons_L;
          Construct PatCons_R;
        ]
        @ pat_int @ pat_bool
    | _ -> [ Construct PatVar; Construct PatWild ]
  in
  match info.current_term with
  | ENode _ -> List.concat [ handle_move (); handle_expr () ]
  | TNode _ -> List.concat [ handle_move (); handle_type () ]
  | PNode _ -> List.concat [ handle_move (); handle_pattern () ]

(*
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
    *)
