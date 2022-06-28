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
[@@deriving sexp]

(* Expressions *)
type node =
  | EVar of Var.t
  | EInt of int
  | EBool of bool
  | EUnOp of unop * t
  | EBinOp of t * binop * t
  | ELet of Var.t * t * t
  | EIf of t * t * t
  | EFun of Var.t * Type.t * t
  | EFix of Var.t * Type.t * t
  | EPair of t * t
  | EHole
  | ENil

and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
}
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
  | EPair_L of z_t * t
  | EPair_R of t * z_t

and z_t = {
  id : int; (* An unique ID assigned to each node *)
  node : z_node; (* Node type and its children *)
}
[@@deriving sexp]

(* Values *)
type value =
  | VInt of int
  | VBool of bool
  | VFun of Var.t * Type.t * t
  | VPair of value * value
  | VNil
  | VError

(* Given a pure node, generate a node with an id *)
let make_node (node : node) : t = { id = Id.generate (); node }
let make_z_node (node : z_node) : z_t = { id = Id.generate (); node }
let make_dummy_node (node : node) : t = { id = -1; node }

(*
    Return the size of the AST
    Input :
      - e : the AST
    Output :
      - the size of the AST
*)
let rec size (e : t) : int =
  match e.node with
  | EVar _ | EInt _ | EBool _ | EHole | ENil -> 1
  | EUnOp (_, e) -> 1 + size e
  | EBinOp (e1, _, e2) | EPair (e1, e2) -> 1 + size e1 + size e2
  | ELet (_, edef, ebody) -> 1 + 1 + size edef + size ebody
  | EIf (econd, ethen, eelse) -> 1 + size econd + size ethen + size eelse
  | EFix (_, ty, ebody) | EFun (_, ty, ebody) ->
      1 + 1 + Type.size ty + size ebody

(* let%test_module "Test Expr.size" =
   (module struct
     let%test _ = size (EInt 10) = 1
     let%test _ = size (EUnOp (OpNeg, EBinOp (EHole, OpPlus, EVar "x"))) = 4

     let%test _ =
       size
         (ELet
            ( "x",
              EIf (EBool true, EInt 3, EInt 4),
              EFun ("f", TProd (TInt, TInt), EBinOp (EVar "f", OpAp, EVar "x"))
            ))
       = 14
   end) *)

(* Get the expression form of a value *)
let rec from_val (v : value) : t =
  let node =
    match v with
    | VInt n -> EInt n
    | VBool b -> EBool b
    | VFun (x, typ, e) -> EFun (x, typ, e)
    | VPair (e1, e2) -> EPair (from_val e1, from_val e2)
    | VNil -> ENil
    | _ -> raise (Failure "Cannot be changed to expr")
  in
  make_node node

(* Convert an unzipped ast into a zipped one, by selecting the root *)
let select_root (e : t) : z_t = { id = e.id; node = Cursor e.node }

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
  | OpAp, OpAp ->
      true
  | _ -> false

let rec equal (t1 : t) (t2 : t) : bool =
  match (t1.node, t2.node) with
  | EVar varn1, EVar varn2 -> Var.equal varn1 varn2
  | EInt val1, EInt val2 -> val1 = val2
  | EBool val1, EBool val2 -> val1 = val2
  | EUnOp (u1, sub1), EUnOp (u2, sub2) -> unop_equal u1 u2 && equal sub1 sub2
  | EBinOp (subl1, b1, subr1), EBinOp (subl2, b2, subr2) ->
      binop_equal b1 b2 && equal subl1 subl2 && equal subr1 subr2
  | ELet (var1, subl1, subr1), ELet (var2, subl2, subr2) ->
      Var.equal var1 var2 && equal subl1 subl2 && equal subr1 subr2
  | EIf (argl1, argc1, argr1), EIf (argl2, argc2, argr2) ->
      equal argl1 argl2 && equal argc1 argc2 && equal argr1 argr2
  | EFun (var1, t1, sub1), EFun (var2, t2, sub2)
  | EFix (var1, t1, sub1), EFix (var2, t2, sub2) ->
      Var.equal var1 var2 && Type.equal t1 t2 && equal sub1 sub2
  | EPair (subl1, subr1), EPair (subl2, subr2) ->
      equal subl1 subl2 && equal subr1 subr2
  | EHole, EHole | ENil, ENil -> true
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
  | _ -> false

let rec unzip (tree : z_t) : t =
  let id = tree.id in
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
  in
  { id; node }
(*unzip child type*)

(* Each edge is represented as (index of start node, index of end node, edge type) *)
(* let%test_module "Test Expr.unzip" =
   (module struct
     let%test _ = unzip (Cursor EHole) = EHole

     let%test _ =
       unzip (EPair_L (Cursor (EInt 7), EBool false))
       = EPair (EInt 7, EBool false)
   end) *)
