(* Defines the expressions of the AST *)
open Sexplib.Std

(* Unary Operators *)
type unop = OpNeg [@@deriving sexp]

(* Binary Operators *)
type binop =
  | OpPlus
  | OpMinus
  | OpTimes
  | OpDiv
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpEq
  | OpNe
  | OpCons
  | OpAp
  | OpAnd
  | OpOr
[@@deriving sexp]

(* Expression with metadata *)
type node =
  | EVar of Var.t
  | EConst of Const.t
  | EUnOp of unop * t
  | EBinOp of t * binop * t
  | ELet of Var.t * t * t
  | EIf of t * t * t
  | EFun of Var.t * Type.t * t
  | EFix of Var.t * Type.t * t
  | EPair of t * t
  | EMap of t * t
  | EFilter of t * t
  | EFold of t * t * t
  | EListEq of t * t
  | EHole
  | EMatch of t * (Pattern.t * t) * (Pattern.t * t)
  | EAssert of t

and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

(* Pure expression *)
type p_t =
  | Var of Var.t
  | Const of Const.t
  | UnOp of unop * p_t
  | BinOp of p_t * binop * p_t
  | Let of Var.t * p_t * p_t
  | If of p_t * p_t * p_t
  | Fun of Var.t * Type.p_t * p_t
  | Fix of Var.t * Type.p_t * p_t
  | Pair of p_t * p_t
  | Map of p_t * p_t
  | Filter of p_t * p_t
  | Fold of p_t * p_t * p_t
  | ListEq of p_t * p_t
  | Hole
  | Match of p_t * (Pattern.p_t * p_t) * (Pattern.p_t * p_t)
  | Assert of p_t
[@@deriving sexp]

(* Zippered Expressions *)
type z_node =
  | Cursor of node
  | EUnOp_L of unop * z_t
  | EBinOp_L of z_t * binop * t
  | EBinOp_R of t * binop * z_t
  | ELet_L of Var.t * z_t * t
  | ELet_R of Var.t * t * z_t
  | EIf_L of z_t * t * t
  | EIf_C of t * z_t * t
  | EIf_R of t * t * z_t
  | EFun_R of Var.t * Type.t * z_t
  | EFun_L of Var.t * Type.z_t * t
  | EFix_R of Var.t * Type.t * z_t
  | EFix_L of Var.t * Type.z_t * t
  | EMap_L of z_t * t
  | EMap_R of t * z_t
  | EFilter_L of z_t * t
  | EFilter_R of t * z_t
  | EFold_L of z_t * t * t
  | EFold_C of t * z_t * t
  | EFold_R of t * t * z_t
  | EListEq_L of z_t * t
  | EListEq_R of t * z_t
  | EPair_L of z_t * t
  | EPair_R of t * z_t
  | EMatch_L of z_t * (Pattern.t * t) * (Pattern.t * t)
  | EMatch_P1 of t * (Pattern.z_t * t) * (Pattern.t * t)
  | EMatch_E1 of t * (Pattern.t * z_t) * (Pattern.t * t)
  | EMatch_P2 of t * (Pattern.t * t) * (Pattern.z_t * t)
  | EMatch_E2 of t * (Pattern.t * t) * (Pattern.t * z_t)
  | EAssert_L of z_t

and z_t = {
  id : int; (* An unique ID assigned to each node *)
  node : z_node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

(* Values *)
type value =
  | VConst of Const.t
  | VFun of Var.t * Type.p_t * p_t
  | VPair of value * value
  | VCons of value * value
  | VError
  | VUnit

(* Given a pure node, generate a node with an id *)
let make_node (node : node) : t = { id = Id.generate (); node; starter = false }

let make_z_node (node : z_node) : z_t =
  { id = Id.generate (); node; starter = false }

let make_dummy_node (node : node) : t = { id = -1; node; starter = false }
let make_dummy_z_node (node : z_node) : z_t = { id = -1; node; starter = false }

(*
   Strip the meta data from the nodes to form a pure expression
*)
let rec strip (e : t) : p_t =
  match e.node with
  | EVar x -> Var x
  | EConst c -> Const c
  | EUnOp (op, e) -> UnOp (op, strip e)
  | EBinOp (e1, op, e2) -> BinOp (strip e1, op, strip e2)
  | ELet (x, e1, e2) -> Let (x, strip e1, strip e2)
  | EIf (e1, e2, e3) -> If (strip e1, strip e2, strip e3)
  | EFun (x, t, e) -> Fun (x, Type.strip t, strip e)
  | EFix (x, t, e) -> Fix (x, Type.strip t, strip e)
  | EMap (e1, e2) -> Map (strip e1, strip e2)
  | EFilter (e1, e2) -> Filter (strip e1, strip e2)
  | EFold (e1, e2, e3) -> Fold (strip e1, strip e2, strip e3)
  | EListEq (e1, e2) -> ListEq (strip e1, strip e2)
  | EPair (e1, e2) -> Pair (strip e1, strip e2)
  | EHole -> Hole
  | EMatch (e, (p1, e1), (p2, e2)) ->
      Match (strip e, (Pattern.strip p1, strip e1), (Pattern.strip p2, strip e2))
  | EAssert e -> Assert (strip e)

let rec add_metadata (e : p_t) : t =
  match e with
  | Var x -> make_node (EVar x)
  | Const c -> make_node (EConst c)
  | UnOp (op, e) -> make_node (EUnOp (op, add_metadata e))
  | BinOp (e1, op, e2) ->
      make_node (EBinOp (add_metadata e1, op, add_metadata e2))
  | Let (x, e1, e2) -> make_node (ELet (x, add_metadata e1, add_metadata e2))
  | If (e1, e2, e3) ->
      make_node (EIf (add_metadata e1, add_metadata e2, add_metadata e3))
  | Fun (x, t, e) -> make_node (EFun (x, Type.add_metadata t, add_metadata e))
  | Fix (x, t, e) -> make_node (EFix (x, Type.add_metadata t, add_metadata e))
  | Pair (e1, e2) -> make_node (EPair (add_metadata e1, add_metadata e2))
  | Map (e1, e2) -> make_node (EMap (add_metadata e1, add_metadata e2))
  | Filter (e1, e2) -> make_node (EFilter (add_metadata e1, add_metadata e2))
  | Fold (e1, e2, e3) ->
      make_node (EFold (add_metadata e1, add_metadata e2, add_metadata e3))
  | ListEq (e1, e2) -> make_node (EListEq (add_metadata e1, add_metadata e2))
  | Hole -> make_node EHole
  | Match (e, (p1, e1), (p2, e2)) ->
      make_node
        (EMatch
           ( add_metadata e,
             (Pattern.add_metadata p1, add_metadata e1),
             (Pattern.add_metadata p2, add_metadata e2) ))
  | Assert e -> make_node (EAssert (add_metadata e))

(*
    Return the size of the AST
    Input :
      - e : the AST
    Output :
      - the size of the AST
*)
let rec size (e : t) : int =
  match e.node with
  | EVar _ | EConst _ | EHole -> 1
  | EUnOp (_, e) | EAssert e -> 1 + size e
  | EBinOp (e1, _, e2) | EPair (e1, e2) -> 1 + size e1 + size e2
  | ELet (_, edef, ebody) -> 1 + 1 + size edef + size ebody
  | EFold (e1, e2, e3) | EIf (e1, e2, e3) -> 1 + size e1 + size e2 + size e3
  | EMap (e1, e2) -> 1 + size e1 + size e2
  | EFilter (e1, e2) -> 1 + size e1 + size e2
  | EListEq (e1, e2) -> 1 + size e1 + size e2
  | EFix (_, ty, ebody) | EFun (_, ty, ebody) ->
      1 + 1 + Type.size ty + size ebody
  | EMatch (e, (p1, e1), (p2, e2)) ->
      1 + size e + Pattern.size p1 + size e1 + Pattern.size p2 + size e2

let%test_module "Test Expr.size" =
  (module struct
    let check e n = size (add_metadata e) = n

    let%test _ = check (Const (Int 10)) 1
    let%test _ = check (UnOp (OpNeg, BinOp (Hole, OpPlus, Var 0))) 4

    let%test _ =
      check
        (Let
           ( 0,
             If (Const (Bool true), Const (Int 3), Const (Int 4)),
             Fun (1, Prod (Int, Int), BinOp (Var 1, OpAp, Var 0)) ))
        14
  end)

(* Get the expression form of a value *)
let rec from_val (v : value) : p_t =
  match v with
  | VConst c -> Const c
  | VFun (x, typ, e) -> Fun (x, typ, e)
  | VPair (e1, e2) -> Pair (from_val e1, from_val e2)
  | VCons (e1, e2) -> BinOp (from_val e1, OpCons, from_val e2)
  | _ -> raise (Failure "Cannot be changed to expr")

(* Convert an unzipped ast into a zipped one, by selecting the root *)
let select_root (e : t) : z_t =
  { id = e.id; node = Cursor e.node; starter = e.starter }

(* Migrate the metadata from Expr.t to Expr.z_t *)
let zip_migrate (e : t) (node : z_node) : z_t =
  { id = e.id; node; starter = e.starter }

let unop_equal (u1 : unop) (u2 : unop) : bool =
  match (u1, u2) with OpNeg, OpNeg -> true

let binop_equal (b1 : binop) (b2 : binop) : bool =
  match (b1, b2) with
  | OpPlus, OpPlus
  | OpMinus, OpMinus
  | OpTimes, OpTimes
  | OpDiv, OpDiv
  | OpLt, OpLt
  | OpLe, OpLe
  | OpGt, OpGt
  | OpGe, OpGe
  | OpEq, OpEq
  | OpNe, OpNe
  | OpCons, OpCons
  | OpAp, OpAp
  | OpAnd, OpAnd
  | OpOr, OpOr ->
      true
  | _ -> false

let rec equal (t1 : t) (t2 : t) : bool =
  match (t1.node, t2.node) with
  | EVar varn1, EVar varn2 -> Var.equal varn1 varn2
  | EConst c1, EConst c2 -> Const.equal c1 c2
  | EUnOp (u1, sub1), EUnOp (u2, sub2) -> unop_equal u1 u2 && equal sub1 sub2
  | EBinOp (subl1, b1, subr1), EBinOp (subl2, b2, subr2) ->
      binop_equal b1 b2 && equal subl1 subl2 && equal subr1 subr2
  | ELet (var1, subl1, subr1), ELet (var2, subl2, subr2) ->
      Var.equal var1 var2 && equal subl1 subl2 && equal subr1 subr2
  | EFold (argl1, argc1, argr1), EFold (argl2, argc2, argr2)
  | EIf (argl1, argc1, argr1), EIf (argl2, argc2, argr2) ->
      equal argl1 argl2 && equal argc1 argc2 && equal argr1 argr2
  | EFun (var1, t1, sub1), EFun (var2, t2, sub2)
  | EFix (var1, t1, sub1), EFix (var2, t2, sub2) ->
      Var.equal var1 var2 && Type.equal t1 t2 && equal sub1 sub2
  | EPair (subl1, subr1), EPair (subl2, subr2) ->
      equal subl1 subl2 && equal subr1 subr2
  | EMap (subl1, subl2), EMap (subr1, subr2)
  | EFilter (subl1, subl2), EFilter (subr1, subr2) ->
      equal subl1 subr1 && equal subl2 subr2
  | EListEq (subl1, subl2), EListEq (subr1, subr2) ->
      equal subl1 subr1 && equal subl2 subr2
  | EHole, EHole -> true
  | EMatch (scrut1, (p1, e1), (p2, e2)), EMatch (scrut2, (p1', e1'), (p2', e2'))
    ->
      equal scrut1 scrut2 && Pattern.equal p1 p1' && equal e1' e2'
      && Pattern.equal p2 p2' && equal e2' e2
  | EAssert sub1, EAssert sub2 -> equal sub1 sub2
  | _ -> false

let rec z_equal (t1 : z_t) (t2 : z_t) : bool =
  match (t1.node, t2.node) with
  | Cursor sub1, Cursor sub2 ->
      equal (make_dummy_node sub1) (make_dummy_node sub2)
  | EUnOp_L (u1, sub1), EUnOp_L (u2, sub2) ->
      unop_equal u1 u2 && z_equal sub1 sub2
  | EBinOp_L (zsub1, b1, sub1), EBinOp_L (zsub2, b2, sub2)
  | EBinOp_R (sub1, b1, zsub1), EBinOp_R (sub2, b2, zsub2) ->
      binop_equal b1 b2 && z_equal zsub1 zsub2 && equal sub1 sub2
  | ELet_L (var1, zsub1, sub1), ELet_L (var2, zsub2, sub2)
  | ELet_R (var1, sub1, zsub1), ELet_R (var2, sub2, zsub2) ->
      Var.equal var1 var2 && z_equal zsub1 zsub2 && equal sub1 sub2
  | EFold_L (zsub1, lsub1, rsub1), EFold_L (zsub2, lsub2, rsub2)
  | EFold_C (lsub1, zsub1, rsub1), EFold_C (lsub2, zsub2, rsub2)
  | EFold_R (lsub1, rsub1, zsub1), EFold_R (lsub2, rsub2, zsub2)
  | EIf_L (zsub1, lsub1, rsub1), EIf_L (zsub2, lsub2, rsub2)
  | EIf_C (lsub1, zsub1, rsub1), EIf_C (lsub2, zsub2, rsub2)
  | EIf_R (lsub1, rsub1, zsub1), EIf_R (lsub2, rsub2, zsub2) ->
      z_equal zsub1 zsub2 && equal lsub1 lsub2 && equal rsub1 rsub2
  | EFun_L (varn1, typ1, sub1), EFun_L (varn2, typ2, sub2)
  | EFix_L (varn1, typ1, sub1), EFix_L (varn2, typ2, sub2) ->
      Var.equal varn1 varn2 && Type.z_equal typ1 typ2 && equal sub1 sub2
  | EFun_R (varn1, typ1, sub1), EFun_R (varn2, typ2, sub2)
  | EFix_R (varn1, typ1, sub1), EFix_R (varn2, typ2, sub2) ->
      Var.equal varn1 varn2 && Type.equal typ1 typ2 && z_equal sub1 sub2
  | EPair_L (zsub1, sub1), EPair_L (zsub2, sub2)
  | EPair_R (sub1, zsub1), EPair_R (sub2, zsub2) ->
      equal sub1 sub2 && z_equal zsub1 zsub2
  | EMap_L (subl1, subl2), EMap_L (subr1, subr2)
  | EFilter_L (subl1, subl2), EFilter_L (subr1, subr2) ->
      z_equal subl1 subr1 && equal subl2 subr2
  | EMap_R (subl1, subl2), EMap_R (subr1, subr2)
  | EFilter_R (subl1, subl2), EFilter_R (subr1, subr2) ->
      equal subl1 subr1 && z_equal subl2 subr2
  | EListEq_L (subl1, subl2), EListEq_L (subr1, subr2) ->
      z_equal subl1 subr1 && equal subl2 subr2
  | EListEq_R (subl1, subl2), EListEq_R (subr1, subr2) ->
      equal subl1 subr1 && z_equal subl2 subr2
  | ( EMatch_L (zsub1, (p1, e1), (p2, e2)),
      EMatch_L (zsub2, (p1', e1'), (p2', e2')) ) ->
      z_equal zsub1 zsub2 && Pattern.equal p1 p1' && equal e1' e2'
      && Pattern.equal p2 p2' && equal e2' e2
  | ( EMatch_P1 (scrut1, (p1, e1), (p2, e2)),
      EMatch_P1 (scrut2, (p1', e1'), (p2', e2')) ) ->
      equal scrut1 scrut2 && Pattern.z_equal p1 p1' && Pattern.equal p2 p2'
      && equal e1 e1' && equal e2 e2'
  | ( EMatch_E1 (scrut1, (p1, e1), (p2, e2)),
      EMatch_E1 (scrut2, (p1', e1'), (p2', e2')) ) ->
      equal scrut1 scrut2 && Pattern.equal p1 p1' && Pattern.equal p2 p2'
      && z_equal e1 e1' && equal e2 e2'
  | ( EMatch_P2 (scrut1, (p1, e1), (p2, e2)),
      EMatch_P2 (scrut2, (p1', e1'), (p2', e2')) ) ->
      equal scrut1 scrut2 && Pattern.equal p1 p1' && Pattern.z_equal p2 p2'
      && equal e1 e1' && equal e2 e2'
  | ( EMatch_E2 (scrut1, (p1, e1), (p2, e2)),
      EMatch_E2 (scrut2, (p1', e1'), (p2', e2')) ) ->
      equal scrut1 scrut2 && Pattern.equal p1 p1' && Pattern.equal p2 p2'
      && equal e1 e1' && z_equal e2 e2'
  | EAssert_L sub1, EAssert_L sub2 -> z_equal sub1 sub2
  | _ -> false

let rec unzip (tree : z_t) : t =
  let node =
    match tree.node with
    | Cursor arg -> arg
    | EUnOp_L (unop, l_child) -> EUnOp (unop, unzip l_child)
    | EBinOp_L (l_child, binop, r_child) ->
        EBinOp (unzip l_child, binop, r_child)
    | EBinOp_R (l_child, binop, r_child) ->
        EBinOp (l_child, binop, unzip r_child)
    | ELet_L (var, l_child, r_child) -> ELet (var, unzip l_child, r_child)
    | ELet_R (var, l_child, r_child) -> ELet (var, l_child, unzip r_child)
    | EFold_L (l, c, r) -> EFold (unzip l, c, r)
    | EFold_C (l, c, r) -> EFold (l, unzip c, r)
    | EFold_R (l, c, r) -> EFold (l, c, unzip r)
    | EIf_L (l, c, r) -> EIf (unzip l, c, r)
    | EIf_C (l, c, r) -> EIf (l, unzip c, r)
    | EIf_R (l, c, r) -> EIf (l, c, unzip r)
    | EPair_L (l, r) -> EPair (unzip l, r)
    | EPair_R (l, r) -> EPair (l, unzip r)
    | EFun_R (var_n, var_t, child) -> EFun (var_n, var_t, unzip child)
    | EFun_L (var_n, var_t, child) ->
        EFun (var_n, Type.unzip var_t, child) (*unzip child type*)
    | EFix_R (var_n, var_t, child) -> EFix (var_n, var_t, unzip child)
    | EFix_L (var_n, var_t, child) -> EFix (var_n, Type.unzip var_t, child)
    | EMap_L (e1, e2) -> EMap (unzip e1, e2)
    | EMap_R (e1, e2) -> EMap (e1, unzip e2)
    | EFilter_L (e1, e2) -> EFilter (unzip e1, e2)
    | EFilter_R (e1, e2) -> EFilter (e1, unzip e2)
    | EListEq_L (e1, e2) -> EListEq (unzip e1, e2)
    | EListEq_R (e1, e2) -> EListEq (e1, unzip e2)
    | EMatch_L (e, rule1, rule2) -> EMatch (unzip e, rule1, rule2)
    | EMatch_P1 (scrut, (p1, e1), rule2) ->
        EMatch (scrut, (Pattern.unzip p1, e1), rule2)
    | EMatch_E1 (scrut, (p1, e1), rule2) -> EMatch (scrut, (p1, unzip e1), rule2)
    | EMatch_P2 (scrut, rule1, (p2, e2)) ->
        EMatch (scrut, rule1, (Pattern.unzip p2, e2))
    | EMatch_E2 (scrut, rule1, (p2, e2)) -> EMatch (scrut, rule1, (p2, unzip e2))
    | EAssert_L child -> EAssert (unzip child)
  in
  { id = tree.id; node; starter = tree.starter }
(*unzip child type*)

let rec add_vars (e : t) : unit =
  match e.node with
  | EUnOp (_, child) -> add_vars child
  | EMap (l_child, r_child)
  | EFilter (l_child, r_child)
  | EBinOp (l_child, _, r_child)
  | EPair (l_child, r_child)
  | EListEq (l_child, r_child) ->
      add_vars l_child;
      add_vars r_child
  | EConst _ | EHole | EVar _ -> ()
  | EAssert child -> add_vars child
  | ELet (x, l_child, r_child) ->
      if x < Var.max_num_vars then Var.used_vars.(x) <- true;
      Var.num_vars := !Var.num_vars + 1;
      add_vars l_child;
      add_vars r_child
  | EFold (l_child, c_child, r_child) | EIf (l_child, c_child, r_child) ->
      add_vars l_child;
      add_vars c_child;
      add_vars r_child
  | EFun (x, _, child) | EFix (x, _, child) ->
      if x < Var.max_num_vars then Var.used_vars.(x) <- true;
      Var.num_vars := !Var.num_vars + 1;
      add_vars child
  | EMatch (e, (p1, e1), (p2, e2)) -> (
      add_vars e;
      match p1.node with
      | PVar x ->
          Var.used_vars.(x) <- true;
          Var.num_vars := !Var.num_vars + 1
      | _ -> (
          ();
          add_vars e1;
          match p2.node with
          | PVar x ->
              Var.used_vars.(x) <- true;
              Var.num_vars := !Var.num_vars + 1
          | _ ->
              ();
              add_vars e2))

(* Each edge is represented as (index of start node, index of end node, edge type) *)
(* let%test_module "Test Expr.unzip" =
   (module struct
     let check z_e e = strip (unzip z_e) = e

     let%test _ = check (make_z_node (Cursor EHole)) Hole

     let%test _ =
       check (EPair_L (Cursor (EInt 7), EBool false)) EPair (EInt 7, EBool false)
   end) *)

let rec set_starter (e : t) (b : bool) : t =
  (* what does this do? *)
  let new_node =
    match e.node with
    | EVar _ | EConst _ | EHole -> e.node
    | EUnOp (unop, child) -> EUnOp (unop, set_starter child b)
    | EBinOp (l_child, binop, r_child) ->
        EBinOp (set_starter l_child b, binop, set_starter r_child b)
    | ELet (var, l_child, r_child) ->
        ELet (var, set_starter l_child b, set_starter r_child b)
    | EIf (l_child, c_child, r_child) ->
        EIf (set_starter l_child b, set_starter c_child b, set_starter r_child b)
    | EFold (l_child, c_child, r_child) ->
        EFold
          (set_starter l_child b, set_starter c_child b, set_starter r_child b)
    | EFun (var, typ, child) -> EFun (var, typ, set_starter child b)
    | EFix (var, typ, child) -> EFix (var, typ, set_starter child b)
    | EPair (l_child, r_child) ->
        EPair (set_starter l_child b, set_starter r_child b)
    | EMap (func, list) -> EMap (set_starter func b, set_starter list b)
    | EFilter (func, list) -> EFilter (set_starter func b, set_starter list b)
    | EListEq (l_child, r_child) ->
        EListEq (set_starter l_child b, set_starter r_child b)
    | EMatch (e, (p1, e1), (p2, e2)) ->
        EMatch
          ( set_starter e b,
            (Pattern.set_starter p1 b, set_starter e1 b),
            (Pattern.set_starter p2 b, set_starter e2 b) )
    | EAssert child -> EAssert (set_starter child b)
  in
  { e with node = new_node; starter = b }
