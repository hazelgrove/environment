(* Sum type combining the expressions and types *)

type t = ENode of Expr.t | TNode of Type.t [@@deriving sexp]
type z_t = ZENode of Expr.z_t | ZTNode of Type.z_t [@@deriving sexp]
type p_t = PENode of Expr.p_t | PTNode of Type.p_t [@@deriving sexp]

let size (tree : t) : int =
  match tree with ENode e -> Expr.size e | TNode t -> Type.size t

let zsize (tree : z_t) : int =
  match tree with
  | ZENode e -> Expr.size (Expr.unzip e)
  | ZTNode t -> Type.size (Type.unzip t)

let equal (tree1 : t) (tree2 : t) : bool =
  match (tree1, tree2) with
  | ENode e1, ENode e2 -> Expr.equal e1 e2
  | TNode t1, TNode t2 -> Type.equal t1 t2
  | _ -> false
