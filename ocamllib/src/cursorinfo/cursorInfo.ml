(* Information at the cursor *)
open Typing

exception TypeError of string

type t = {
  current_term : Syntax.t;
  (*the currently focussed term (use to decide whether we can go down) *)
  parent_term : Syntax.z_t option;
  (* parent of current term (use to decide whether we can go up)  *)
  vars_in_scope : (Var.t * int) list;
  (* variables in scope *)
  typ_ctx : Context.t;
  (*mapping of vars in scope to types (use to determine vars in scope)    *)
  expected_ty : Type.p_t option;
  (* analyzed type of cursor_term; build up through recursion (use with ctx to determine viable insert actions) *)
  actual_ty : Type.p_t option;
  (* result of calling Syn on current_term (use to determine wrapping viability)  *)
  cursor_position : int; (* Index of the cursor *)
}

(*
   Given a zippered AST, return the information (expr, typing, etc.) at the cursor
   For variables in scope, assumes that there is no shadowing
*)
let get_cursor_info (e : Syntax.z_t) : t =
  let rec get_cursor_info_type ~(current_term : Type.z_t)
      ~(parent_term : Syntax.z_t option) ~(index : int) =
    match current_term.node with
    (* TODO: Add exp_ty & actual_ty *)
    | Cursor t ->
        let t : Type.t = { id = current_term.id; node = t } in
        (* No variables for types, so vars_in_scope & typ_ctx are [] *)
        {
          current_term = TNode t;
          parent_term;
          vars_in_scope = [];
          typ_ctx = [];
          expected_ty = None;
          actual_ty = None;
          cursor_position = index;
        }
    | Arrow_L (t, _) | Prod_L (t, _) | List_L t ->
        get_cursor_info_type ~current_term:t
          ~parent_term:(Some (ZTNode current_term)) ~index:(index + 1)
    | Arrow_R (t1, t2) | Prod_R (t1, t2) ->
        get_cursor_info_type ~current_term:t2
          ~parent_term:(Some (ZTNode current_term))
          ~index:(index + Type.size t1 + 1)
  in
  let rec get_cursor_info_expr ~(current_term : Expr.z_t)
      ~(parent_term : Expr.z_t option) ~(vars : (Var.t * int) list)
      ~(typ_ctx : Context.t) ~(exp_ty : Type.p_t) ~(index : int) =
    match current_term.node with
    | Cursor e -> (
        let e : Expr.t = { id = current_term.id; node = e } in
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
              typ_ctx;
              expected_ty = Some exp_ty;
              actual_ty = Some t;
              cursor_position = index;
            }
        | None -> raise (TypeError "Incorrec type"))
    | EUnOp_L (OpNeg, e) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty:Type.Int ~index:(index + 1)
    | EBinOp_L
        ( e,
          ( OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe
          | OpEq | OpNe ),
          _ ) ->
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty:Type.Int ~index:(index + 1)
    | EBinOp_R
        ( e1,
          ( OpPlus | OpMinus | OpTimes | OpDiv | OpGt | OpGe | OpLt | OpLe
          | OpEq | OpNe ),
          e2 ) ->
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty:Type.Int
          ~index:(index + Expr.size e1 + 1)
    | EBinOp_L (e1, OpCons, e2) ->
        let exp_ty =
          match synthesis typ_ctx e2 with
          | Some Type.Hole -> Type.Hole
          | Some (Type.List t) -> t
          | _ -> raise (TypeError "Expected a list type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty ~index:(index + 1)
    | EBinOp_R (e1, OpCons, e2) ->
        let exp_ty =
          match synthesis typ_ctx e1 with
          | Some t -> Type.List t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | EBinOp_L (e1, OpAp, e2) ->
        let exp_ty =
          match synthesis typ_ctx e2 with
          | Some t -> Type.Arrow (t, exp_ty)
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty ~index:(index + 1)
    | EBinOp_R (e1, OpAp, e2) ->
        let exp_ty =
          match synthesis typ_ctx e1 with
          | Some Type.Hole -> Type.Hole
          | Some (Type.Arrow (tin, tout)) -> tin
          | _ -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
    | ELet_L (_, edef, ebody) ->
        get_cursor_info_expr ~current_term:edef ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty:Type.Hole ~index:(index + 2)
    | ELet_R (x, edef, ebody) ->
        let x_type =
          match synthesis typ_ctx edef with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:ebody
          ~parent_term:(Some current_term)
          ~vars:((x, index + 1) :: vars)
          ~typ_ctx:(Context.extend typ_ctx (x, x_type))
          ~exp_ty:Type.Hole
          ~index:(index + Expr.size edef + 2)
    | EIf_L (econd, _, _) ->
        get_cursor_info_expr ~current_term:econd
          ~parent_term:(Some current_term) ~vars ~typ_ctx ~exp_ty:Type.Bool
          ~index:(index + 1)
    | EIf_C (econd, ethen, eelse) ->
        let exp_ty =
          match synthesis typ_ctx eelse with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:ethen
          ~parent_term:(Some current_term) ~vars ~typ_ctx ~exp_ty
          ~index:(index + Expr.size econd + 1)
    | EIf_R (econd, ethen, eelse) ->
        let exp_ty =
          match synthesis typ_ctx ethen with
          | Some t -> t
          | None -> raise (TypeError "Type cannot be inferred")
        in
        get_cursor_info_expr ~current_term:eelse
          ~parent_term:(Some current_term) ~vars ~typ_ctx ~exp_ty
          ~index:(index + Expr.size econd + Expr.size ethen + 1)
    | EFun_L (_, t, _) | EFix_L (_, t, _) ->
        get_cursor_info_type ~current_term:t
          ~parent_term:(Some (Syntax.ZENode current_term)) ~index:(index + 1)
    | EFun_R (x, t, e) | EFix_R (x, t, e) ->
        let exp_ty =
          match exp_ty with
          | Type.Arrow (tin, tout) -> tout
          | Type.Hole -> Type.Hole
          | _ -> raise (TypeError "Expected a function type")
        in
        get_cursor_info_expr ~current_term:e ~parent_term:(Some current_term)
          ~vars:((x, index + 1) :: vars)
          ~typ_ctx:(Context.extend typ_ctx (x, Type.strip t))
          ~exp_ty
          ~index:(index + Type.size t + 2)
    | EPair_L (e1, e2) ->
        let exp_ty =
          match exp_ty with
          | Type.Prod (t1, t2) -> t1
          | Type.Hole -> Type.Hole
          | _ -> raise (TypeError "Expected a function type")
        in
        get_cursor_info_expr ~current_term:e1 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty ~index:(index + 1)
    | EPair_R (e1, e2) ->
        let exp_ty =
          match exp_ty with
          | Type.Prod (t1, t2) -> t2
          | Type.Hole -> Type.Hole
          | _ -> raise (TypeError "Expected a function type")
        in
        get_cursor_info_expr ~current_term:e2 ~parent_term:(Some current_term)
          ~vars ~typ_ctx ~exp_ty
          ~index:(index + Expr.size e1 + 1)
  in
  match e with
  | ZENode e ->
      get_cursor_info_expr ~current_term:e ~parent_term:None ~vars:[]
        ~typ_ctx:[] ~exp_ty:Type.Hole ~index:0
  | ZTNode t -> get_cursor_info_type ~current_term:t ~parent_term:None ~index:0

let%test_module "Test get_cursor_info" =
  (module struct
    let equal i i' =
      Syntax.equal i.current_term i'.current_term
      && (match (i.parent_term, i'.parent_term) with
         | Some _, Some _ | None, None -> true
         | _ -> false)
      && i.vars_in_scope = i'.vars_in_scope
      && i.typ_ctx = i'.typ_ctx
      && i.expected_ty = i'.expected_ty
      && i.actual_ty = i'.actual_ty
      && i.cursor_position = i'.cursor_position

    let check e i = equal (get_cursor_info (ZENode e)) i
    let e : Expr.z_t = { id = -1; node = Expr.Cursor EHole }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node Expr.EHole);
        parent_term = None;
        vars_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Hole;
        cursor_position = 0;
      }

    let%test _ = check e i

    let e : Expr.z_t = { id = -1; node = Expr.Cursor (EInt 1) }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node (EInt 1));
        parent_term = None;
        vars_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Int;
        cursor_position = 0;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node = Expr.EUnOp_L (OpNeg, { id = -1; node = Expr.Cursor (EBool true) });
      }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node (Expr.EBool true));
        parent_term = Some (ZENode e);
        vars_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Int;
        actual_ty = Some Type.Bool;
        cursor_position = 1;
      }

    let%test _ = check e i

    (* Remove checks on current & parent terms *)
    let equal i i' =
      i.vars_in_scope = i'.vars_in_scope
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
            ( "x",
              Expr.make_dummy_node (EInt 1),
              { id = -1; node = Expr.Cursor (EVar "x") } );
      }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [ ("x", 1) ];
        typ_ctx = [ ("x", Type.Int) ];
        expected_ty = Some Type.Hole;
        actual_ty = Some Type.Int;
        cursor_position = 3;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.ELet_R
            ( "x",
              Expr.make_dummy_node (EInt 1),
              {
                id = -1;
                node =
                  Expr.ELet_R
                    ( "y",
                      Expr.make_dummy_node (EBool false),
                      {
                        id = -1;
                        node =
                          Expr.EBinOp_L
                            ( Expr.make_z_node (Expr.Cursor (EVar "y")),
                              OpAp,
                              Expr.make_dummy_node (EInt 2) );
                      } );
              } );
      }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [ ("y", 4); ("x", 1) ];
        typ_ctx = [ ("y", Type.Bool); ("x", Type.Int) ];
        expected_ty = Some (Type.Arrow (Type.Int, Type.Hole));
        actual_ty = Some Type.Bool;
        cursor_position = 7;
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
      }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Bool;
        actual_ty = Some Type.Hole;
        cursor_position = 1;
      }

    let%test _ = check e i

    let e : Expr.z_t =
      {
        id = -1;
        node =
          Expr.EIf_C
            ( Expr.make_dummy_node EHole,
              Expr.make_z_node (Cursor (EBool true)),
              Expr.make_dummy_node (EInt 1) );
      }

    let i =
      {
        current_term = ENode (Expr.make_dummy_node EHole);
        parent_term = None;
        vars_in_scope = [];
        typ_ctx = [];
        expected_ty = Some Type.Int;
        actual_ty = Some Type.Bool;
        cursor_position = 2;
      }

    let%test _ = check e i
  end)

let ints =
  [
    Action.Construct (Int (-2));
    Construct (Int (-1));
    Construct (Int 0);
    Construct (Int 1);
    Construct (Int 2);
  ]

let bools = [ Action.Construct (Bool true); Construct (Bool false) ]

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

(* Given the info at the cursor, return a list of possible actions *)
let cursor_info_to_actions (info : t) : Action.t list =
  let open Action in
  let handle_move _ =
    let handle_parent _ =
      match info.parent_term with Some _ -> [ Move Parent ] | None -> []
    in
    let handle_child _ =
      match info.current_term with
      | ENode e -> (
          match e.node with
          | EVar _ | EInt _ | EBool _ | EHole | ENil -> []
          | EUnOp _ -> [ Move (Child 0) ]
          | EBinOp _ | EFun _ | EFix _ | EPair _ ->
              [ Move (Child 0); Move (Child 1) ]
          | EIf _ -> [ Move (Child 0); Move (Child 1); Move (Child 2) ]
          | ELet (_, edef, _) -> (
              match synthesis info.typ_ctx edef with
              | Some Type.Hole -> [ Move (Child 0) ]
              | Some _ -> [ Move (Child 0); Move (Child 1) ]
              | None -> raise (TypeError "Type cannot be inferred")))
      | TNode t -> (
          match t.node with
          | TInt | TBool | THole -> []
          | TList _ -> [ Move (Child 0) ]
          | TArrow _ | TProd _ -> [ Move (Child 0); Move (Child 1) ])
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
      | Type.List _ -> [ Construct Nil; Construct Hole ]
      | Type.Hole -> [ Construct Nil; Construct Hole ] @ ints @ bools
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
      let construct_arith_comp _ =
        match exp_ty with
        | Type.Int -> (
            match actual_ty with Type.Int | Type.Hole -> arith | _ -> [])
        | Type.Bool -> (
            match actual_ty with Type.Bool | Type.Hole -> comp | _ -> [])
        | Type.Hole -> (
            match actual_ty with
            | Type.Int | Type.Hole -> arith @ comp
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
        | Type.Hole ->
            [ Construct (BinOp_L OpCons); Construct (BinOp_R OpCons) ]
        | _ -> []
      in
      construct_arith_comp () @ construct_ap () @ construct_cons ()
    in
    let construct_let _ = [ Construct (Let_L "") ] in
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
      then [ Construct (Fun ""); Construct (Fix "") ]
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
    List.concat
      [
        construct_atom ();
        construct_unop ();
        construct_binop ();
        construct_let ();
        construct_if ();
        construct_fun_fix ();
        construct_pair ();
      ]
  in
  let handle_type _ = [] in
  List.concat [ handle_move (); handle_expr (); handle_type () ]
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
     end) *)
