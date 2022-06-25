(* Sum type combining the expressions and types *)

type t = ENode of Expr.t | TNode of Type.t [@@deriving sexp]
type z_t = ZENode of Expr.z_t | ZTNode of Type.z_t [@@deriving sexp]

let rec size (tree : t) : int =
  match tree with
  | ENode (EVar _ | EInt _ | EBool _ | EHole | ENil) -> 1
  | ENode (EUnOp (_, arg)) -> size (ENode arg) + 1
  | ENode (EBinOp (argl, _, argr) | ELet (_, argl, argr) | EPair (argl, argr))
    ->
      size (ENode argl) + size (ENode argr) + 1
  | ENode (EIf (argl, argc, argr)) ->
      size (ENode argl) + size (ENode argc) + size (ENode argr) + 1
  | ENode (EFun (_, typ, arg) | EFix (_, typ, arg)) ->
      Type.size typ + size (ENode arg) + 1
  | TNode type_tree -> Type.size type_tree

let rec zsize (tree : z_t) : int =
  match tree with
  | ZENode (Cursor cursed) -> size (ENode cursed)
  | ZENode (EUnOp_L (_, argl)) -> zsize (ZENode argl) + 1
  | ZENode (EBinOp_L (argl, _, argr)) ->
      zsize (ZENode argl) + size (ENode argr) + 1
  | ZENode (EBinOp_R (argl, _, argr)) ->
      size (ENode argl) + zsize (ZENode argr) + 1
  | ZENode (ELet_L (_, argl, argr)) ->
      zsize (ZENode argl) + size (ENode argr) + 1
  | ZENode (ELet_R (_, argl, argr)) ->
      size (ENode argl) + zsize (ZENode argr) + 1
  | ZENode (EIf_L (argl, argc, argr)) ->
      zsize (ZENode argl) + size (ENode argc) + size (ENode argr) + 1
  | ZENode (EIf_C (argl, argc, argr)) ->
      size (ENode argl) + zsize (ZENode argc) + size (ENode argr) + 1
  | ZENode (EIf_R (argl, argc, argr)) ->
      size (ENode argl) + size (ENode argc) + zsize (ZENode argr) + 1
  | ZENode (EFun_L (_, typ, argr)) -> size (ENode argr) + size (ENode argr) + 1
  | ZENode (EFun_R (_, typ, argr)) -> size (TNode typ) + zsize (ZENode argr) + 1
  | ZENode (EFix_L (_, typ, argr)) -> zsize (ZTNode typ) + size (ENode argr) + 1
  | ZENode (EFix_R (_, typ, argr)) -> size (TNode typ) + zsize (ZENode argr) + 1
  | ZENode (EPair_L (argl, argr)) -> zsize (ZENode argl) + size (ENode argr) + 1
  | ZENode (EPair_R (argl, argr)) -> size (ENode argl) + zsize (ZENode argr) + 1
  | ZTNode type_tree -> Type.size (Type.unzip type_tree)
