(* Synthesis and Analysis *)

let rec synthesis (context : Context.t) (e : Expr.t) : Type.t option =
  (*given an expression and its type-context, infer its type by looking
     at its children, if possible *)
  match e with
  (*match epxression based on type (and operation for unops and binops)*)
  | EVar x -> Context.lookup context x
  | EInt _ -> Some TInt
  | EBool _ -> Some TBool
  | EUnOp (OpNeg, arg) -> 
      (* negation: if child is int, expr has same type*)
      if analysis context arg Type.TInt then Some TInt else None
  | EBinOp (argl, (OpPlus | OpMinus | OpTimes | OpDiv), argr) ->
      (*arithmetic operations: if we see an int, return an int *)
      if analysis context argl TInt && analysis context argr TInt
      then Some TInt
      else None
  | EBinOp (argl, (OpGt | OpGe | OpLt | OpLe), argr) ->
      (*comparasons: int-> bool*)
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
      | Some var_t -> synthesis (Context.extend context (varn, var_t)) body
      | _ -> None)
  | EFun (varn, vart, body) -> (
      match synthesis (Context.extend context (varn, vart)) body with
      | Some outtype -> Some (TArrow (vart, outtype))
      | _ -> None)
  | EFix (varn, vart, body) ->
      if analysis (Context.extend context (varn, vart)) body vart
      then Some vart
      else None
  | EHole -> Some THole
  | ENil -> Some (TList THole)

and analysis (context : Context.t) (e : Expr.t) (targ : Type.t) : bool =
  (* given an epxression and an expected type,
      return a bool representing whether that's correct*)
  match e with
  | EFun (varn, vart, expr) -> (
      match synthesis (Context.extend context (varn, vart)) expr with
      | Some etyp -> Type.consistent etyp targ
      | None -> false)
  | EFix (varn, vart, arg) ->
      Type.consistent vart targ && analysis context arg targ
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
      | Some vart -> analysis (Context.extend context (varn, vart)) body targ
      | None -> false)
  | _ -> (
      match synthesis context e with
      | None -> false
      | Some expt -> Type.consistent expt targ)
(* this handles all the other cases*)
