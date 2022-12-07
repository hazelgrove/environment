(* Synthesis and Analysis *)

open Type

let rec get_common_type (t1 : Type.p_t) (t2 : Type.p_t) : Type.p_t option =
  if Type.consistent t1 t2
  then
    match (t1, t2) with
    | Type.Hole, t | t, Type.Hole -> Some t
    | Type.Int, Type.Int -> Some Type.Int
    | Type.Bool, Type.Bool -> Some Type.Bool
    | Type.Arrow (t1_l, t1_r), Type.Arrow (t2_l, t2_r) -> (
        let t1 = get_common_type t1_l t2_l in
        let t2 = get_common_type t1_r t2_r in
        match (t1, t2) with
        | Some t1, Some t2 -> Some (Type.Arrow (t1, t2))
        | _ -> None)
    | Type.Prod (t1_l, t1_r), Type.Prod (t2_l, t2_r) -> (
        let t1 = get_common_type t1_l t2_l in
        let t2 = get_common_type t1_r t2_r in
        match (t1, t2) with
        | Some t1, Some t2 -> Some (Type.Prod (t1, t2))
        | _ -> None)
    | Type.List t1, Type.List t2 -> (
        let t = get_common_type t1 t2 in
        match t with Some t -> Some (Type.List t) | _ -> None)
    | Type.Unit, Type.Unit -> Some Type.Unit
    | _ -> None
  else None

let%test_module "Test Typing.get_common_type" =
  (module struct
    let check t1 t2 t =
      match (get_common_type t1 t2, t) with
      | Some t1, Some t2 -> t1 = t2
      | None, None -> true
      | _ -> false

    let%test _ = check Int Hole (Some Int)
    let%test _ = check Int (Arrow (Hole, Hole)) None

    let%test _ =
      check (Prod (Hole, Bool)) (Prod (Int, Hole)) (Some (Prod (Int, Bool)))

    let%test _ = check (List Int) (List Bool) None
  end)

(* Currently Incorrect!!!! *)
let rec pattern_type (p : Pattern.p_t) : Type.p_t =
  match p with
  | Const (Int _) -> Type.Int
  | Const (Bool _) -> Type.Bool
  | Const Nil -> Type.List Type.Hole
  | Var _ -> Type.Hole
  | Cons (p1, p2) -> (
      let t1 = pattern_type p1 in
      let t2 = pattern_type p2 in
      match t2 with
      | Type.Hole -> Type.List t1
      | Type.List t2 -> (
          let t = get_common_type t1 t2 in
          match t with
          | Some t -> Type.List t
          | None ->
              raise
                (Failure
                   ("List pattern type mismatch: " ^ TypeConv.to_string t1
                  ^ " and " ^ TypeConv.to_string t2))
          (* | None -> Type.Hole) *))
      | _ ->
          raise
            (Failure
               ("Second pattern in list pattern is not a list: "
              ^ TypeConv.to_string t2))
      (* | _ -> Type.Hole) *))
  | Wild -> Type.Hole

let%test_module "Test Typing.pattern_type" =
  (module struct
    let check p t = pattern_type p = t

    let%test _ = check (Const (Int 1)) Int
    let%test _ = check (Var 3) Hole
    let%test _ = check (Cons (Const (Int 0), Wild)) (List Int)
    let%test _ = check (Cons (Wild, Var 1)) (List Hole)
    let%test _ = check (Cons (Const (Int 0), Var 1)) (List Int)
    let%test _ = check (Cons (Wild, Wild)) (List Hole)
  end)

(* Return Some t if t is the common type of the rules. Otherwise, return None *)
let pattern_common_type (p1 : Pattern.t) (p2 : Pattern.t) : Type.p_t option =
  get_common_type
    (p1 |> Pattern.strip |> pattern_type)
    (p2 |> Pattern.strip |> pattern_type)

let rec synthesis (context : Context.t) (e : Expr.t) : Type.p_t option =
  (*given an expression and its type-context, infer its type by looking
     at its children, if possible *)
  match e.node with
  (*match epxression based on type (and operation for unops and binops)*)
  | EVar x -> Context.lookup context x
  | EConst c -> (
      match c with
      | Int _ -> Some Int
      | Bool _ -> Some Bool
      | Nil -> Some (List Hole))
  | EUnOp (OpNeg, arg) ->
      (* negation: if child is int, expr has same type *)
      if analysis context arg Int then Some Int else None
  | EBinOp (argl, (OpPlus | OpMinus | OpTimes | OpDiv), argr) ->
      (*arithmetic operations: if we see an int, return an int *)
      if analysis context argl Int && analysis context argr Int
      then Some Int
      else None
  | EBinOp (argl, (OpGt | OpGe | OpLt | OpLe | OpEq | OpNe), argr) ->
      (*comparasons: int-> bool*)
      if analysis context argl Int && analysis context argr Int
      then Some Bool
      else None
  | EBinOp (argl, (OpAnd | OpOr), argr) ->
      (*boolean operations: bool->bool*)
      if analysis context argl Bool && analysis context argr Bool
      then Some Bool
      else None
  | EBinOp (arrow, OpAp, arg) -> (
      match synthesis context arrow with
      | Some (Arrow (in_t, out_t)) ->
          if analysis context arg in_t then Some out_t else None
      | Some Hole -> if analysis context arg Hole then Some Hole else None
      | _ -> None)
  | EBinOp (hd, OpCons, tl) -> (
      match (synthesis context hd, synthesis context tl) with
      | Some t1, Some (List t2) -> (
          match get_common_type t1 t2 with
          | Some t -> Some (List t)
          | None -> None)
      | Some t, Some Hole -> Some (List t)
      | _ -> None)
  | EPair (l_pair, r_pair) -> (
      match (synthesis context l_pair, synthesis context r_pair) with
      | Some l_t, Some r_t -> Some (Prod (l_t, r_t))
      | _ -> None)
  | EIf (econd, ethen, eelse) ->
      if analysis context econd Bool
      then
        let tthen = synthesis context ethen in
        let telse = synthesis context eelse in
        match (tthen, telse) with
        | Some tthen, Some teelse -> get_common_type tthen teelse
        | _ -> None
      else None
  | ELet (varn, dec, body) -> (
      match synthesis context dec with
      | Some var_t -> synthesis (Context.extend context (varn, var_t)) body
      | _ -> None)
  | EMap (left, right) -> (
      match synthesis context left with
      | Some Hole ->
          if analysis context right (Type.List Hole)
          then Some (List Hole)
          else None
      | Some (Type.Arrow (intype, outtype)) ->
          if analysis context right (Type.List intype)
          then Some (List outtype)
          else None
      | _ -> None)
  | EFilter (func, list) -> (
      match synthesis context list with
      | Some Hole ->
          if analysis context func (Type.Arrow (Hole, Bool))
          then Some (List Hole)
          else None
      | Some (List listtype) ->
          if analysis context func (Type.Arrow (listtype, Bool))
          then Some (List listtype)
          else None
      | _ -> None)
  | EListEq (left, right) -> (
      match (synthesis context left, synthesis context right) with
      | Some (List ltype), Some (List rtype) -> (
          match get_common_type ltype rtype with
          | Some t -> Some Bool
          | None -> None)
      | _ -> None)
  | EFun (varn, vart, body) -> (
      let vart = Type.strip vart in
      match synthesis (Context.extend context (varn, vart)) body with
      | Some outtype -> Some (Arrow (vart, outtype))
      | _ -> None)
  | EFix (varn, vart, body) ->
      let vart = Type.strip vart in
      if analysis (Context.extend context (varn, vart)) body vart
      then Some vart
      else None
  | EHole -> Some Hole
  | EFold (func,acc,list) -> 
    ( match (synthesis context func, synthesis context acc,synthesis context list) with
    | Some Hole, Some Hole, Some Hole -> Some Hole
    | Some (Type.Arrow(intype, Type.Arrow(functype,outtype))), Some Hole, Some Hole -> 
        get_common_type intype outtype
    | Some (Type.Arrow(intype, Type.Arrow(functype,outtype))), Some aggt, Some Hole -> 
        if Type.consistent functype aggt then get_common_type intype outtype else None 
    | Some (Type.Arrow(intype, Type.Arrow(functype,outtype))), Some aggt, Some (List listtype) -> 
        if (Type.consistent functype aggt) && (Type.consistent intype listtype)  
            then get_common_type intype outtype else None
    | Some Hole, Some aggtype, Some _ -> Some aggtype
    | _ -> None
    )
| EMatch (escrut, (p1, e1), (p2, e2)) -> (
      match synthesis context escrut with
      | Some tscrut -> (
          let trules = pattern_common_type p1 p2 in
          match trules with
          | Some trules ->
              if Type.consistent tscrut trules
              then
                let t1 = get_rule_type p1 e1 context trules in
                let t2 = get_rule_type p2 e2 context trules in
                match (t1, t2) with
                | Some t1, Some t2 -> get_common_type t1 t2
                | _ -> None
              else None
          | None -> None)
      | None -> None)
  | EAssert e -> if analysis context e Bool then Some Unit else None

and analysis (context : Context.t) (e : Expr.t) (targ : Type.p_t) : bool =
  (* given an epxression and an expected type,
      return a bool representing whether that's correct*)
  match e.node with
  | EFun (varn, vart, expr) -> (
      let vart = Type.strip vart in
      match synthesis (Context.extend context (varn, vart)) expr with
      | Some etyp -> Type.consistent (Arrow (vart, etyp)) targ
      | None -> false)
  | EFix (varn, vart, arg) ->
      let vart = Type.strip vart in
      Type.consistent vart targ && analysis context arg targ
  | EPair (lpair, rpair) -> (
      match targ with
      | Prod (l_t, r_t) ->
          analysis context lpair l_t && analysis context rpair r_t
      | Hole -> analysis context lpair Hole && analysis context rpair Hole
      | _ -> false)
  | EIf (argl, argc, argr) ->
      (* for if statements, first arg is expected to be a bool,
         and second and third are expected to match *)
      if analysis context argl Bool
      then
        match (synthesis context argc, synthesis context argr) with
        | Some t1, Some t2 ->
            Type.consistent t1 targ && Type.consistent t2 targ
            && Type.consistent t1 t2
        | _ -> false
      else false
  | ELet (varn, def, body) -> (
      (* for variable definitions, add variable type to context*)
      match synthesis context def with
      | Some vart -> analysis (Context.extend context (varn, vart)) body targ
      | None -> false)
  | EMap (func, list) -> (
      match (targ, synthesis context list) with
      | Hole, Some Hole -> analysis context func (Type.Arrow (Hole, Hole))
      | Hole, Some (List intype) ->
          analysis context func (Type.Arrow (intype, Hole))
      | List outtype, Some Hole ->
          analysis context func (Type.Arrow (Hole, outtype))
      | List outtype, Some (List intype) ->
          analysis context func (Type.Arrow (intype, outtype))
      | _ -> false)
  | EFilter (func, list) -> (
      match (targ, synthesis context list) with
      | Hole, Some Hole -> analysis context func (Type.Arrow (Hole, Bool))
      | Hole, Some (List intype) ->
          analysis context func (Type.Arrow (intype, Bool))
      | List outtype, Some Hole ->
          analysis context func (Type.Arrow (Hole, Bool))
      | List outtype, Some (List intype) ->
          analysis context func (Type.Arrow (intype, Bool))
          && Type.consistent intype outtype
      | _ -> false)
  | EFold (func,acc, list) -> (
    match (targ, synthesis context func) with 
    | Hole, Some Hole -> 
        analysis context acc Hole  && analysis context list (List Hole)
    | Hole, Some (Type.Arrow(intype, Type.Arrow(functype, outtype))) -> 
        (analysis context acc intype) 
        && (analysis context list (Type.List functype))
        && (Type.consistent intype outtype)
    | targtype, Some Hole -> 
        analysis context acc targtype  && analysis context list (List Hole)
    | targtype, Some (Type.Arrow(intype, Type.Arrow(functype, outtype))) -> 
        Type.consistent intype targtype 
        && Type.consistent outtype targtype 
        && analysis context acc targtype 
        && analysis context list (List functype)
    |_ -> false 
  )
  | _ -> (
      match synthesis context e with
      | None -> false
      | Some expt -> Type.consistent expt targ)
(* this handles all the other cases*)

and get_rule_type (p : Pattern.t) (e : Expr.t) (ctx : Context.t) (t : Type.p_t)
    : Type.p_t option =
  let rec find_ctx (ctx : Context.t) (p : Pattern.t) (t : Type.p_t) : Context.t
      =
    match p.node with
    | PVar x -> Context.extend ctx (x, t)
    | PCons (p1, p2) -> (
        match t with
        | List t ->
            let ctx1 = find_ctx [] p1 t in
            let ctx2 = find_ctx [] p2 (List t) in
            Context.concat ctx (Context.concat ctx1 ctx2)
        | _ -> raise (Failure "Pattern type error"))
    | _ -> ctx
  in
  let ctx = find_ctx ctx p t in
  synthesis ctx e

let%test_module "Test Typing.synthesis & Typing.analysis" =
  (module struct
    let check_syn ctx e t =
      let e = e |> ParserUtils.parse |> Expr.add_metadata in
      match (synthesis ctx e, t) with
      | Some t1, Some t2 -> t1 = t2
      | None, None -> true
      | _ -> false

    let%test _ = check_syn Context.empty "2 :: 3 :: []" (Some (List Int))
    let%test _ = check_syn Context.empty "let x1 = 2 in x1 + 2" (Some Int)
    let%test _ = check_syn Context.empty "if true then 3 else ?" (Some Int)
    let%test _ = check_syn Context.empty "if true then 3 else true" None

    let%test _ =
      check_syn Context.empty "let f = fun x1 -> x1 + 3 in f ?" (Some Int)

    let%test _ =
      check_syn Context.empty "let f = fun x1 -> x1 + 3 in f true" (Some Int)

    let%test _ =
      check_syn Context.empty
        "match 2 :: ? :: [] with | x1 :: x2 :: [] -> 2 | _ -> 1" (Some Int)
  end)
