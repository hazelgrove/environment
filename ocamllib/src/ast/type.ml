(* Basic types *)

open Sexplib.Std

exception SyntaxError of string
exception RuntimeError of string
exception TranslationError of string
exception NotImplemented

type node =
  | TInt
  | TBool
  | TArrow of t * t
  | TProd of t * t
  | THole
  | TList of t
(* temporarily include lists *)

and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
}
[@@deriving sexp]

type z_node =
  | Cursor of node
  | Arrow_L of z_t * t
  | Arrow_R of t * z_t
  | Prod_L of z_t * t
  | Prod_R of t * z_t
  | List_L of z_t

and z_t = {
  id : int; (* An unique ID assigned to each node *)
  node : z_node; (* Node type and its children *)
}
[@@deriving sexp]

let make_node (node : node) : t = { id = Id.generate (); node }
let make_z_node (node : z_node) : z_t = { id = Id.generate (); node }
let make_dummy_node (node : node) : t = { id = -1; node }

(* Check if two types are equal *)
let rec equal (ty : t) (ty' : t) : bool =
  match (ty.node, ty'.node) with
  | TInt, TInt | TBool, TBool | THole, THole -> true
  | TArrow (t1, t2), TArrow (t1', t2') | TProd (t1, t2), TProd (t1', t2') ->
      equal t1 t1' && equal t2 t2'
  | TList t_1, TList t_2 -> equal t_1 t_2
  | _ -> false

(* Check if two zippered types are equal *)
let rec z_equal (ty : z_t) (ty' : z_t) : bool =
  match (ty.node, ty'.node) with
  | Cursor s1, Cursor s2 -> equal (make_dummy_node s1) (make_dummy_node s2)
  | Arrow_L (zt1, t1), Arrow_L (zt2, t2)
  | Arrow_R (t1, zt1), Arrow_R (t2, zt2)
  | Prod_L (zt1, t1), Prod_L (zt2, t2)
  | Prod_R (t1, zt1), Prod_R (t2, zt2) ->
      equal t1 t2 && z_equal zt1 zt2
  | List_L t1, List_L t2 -> z_equal t1 t2
  | _ -> false

(* Check type consistency *)
let rec consistent (ty : t) (ty' : t) : bool =
  match (ty.node, ty'.node) with
  | _, THole | THole, _ -> true
  | TInt, TInt | TBool, TBool -> true
  | TArrow (t1, t2), TArrow (t1', t2') | TProd (t1, t2), TProd (t1', t2') ->
      consistent t1 t1' && consistent t2 t2'
  | TList t_1, TList t_2 -> consistent t_1 t_2
  | _ -> false

(*
    Return the size of the Type Tree
    Input :
      - e : the Type Tree
    Output :
      - the size of the Type Tree
*)
let rec size (ty : t) : int =
  match ty.node with
  | TInt | TBool | THole -> 1
  | TList t1 -> 1 + size t1
  | TArrow (t1, t2) | TProd (t1, t2) -> 1 + size t1 + size t2

(* let%test_module "Test Typ.size" =
   (module struct
     let%test _ = size (TArrow (TProd (TInt, TInt), TBool)) = 5
     let%test _ = size (TArrow (TArrow (TInt, TInt), TProd (TBool, THole))) = 7
   end) *)

let rec unzip (tree : z_t) : t =
  let id = tree.id in
  let node : node =
    match tree.node with
    | Cursor subtree -> subtree
    | Arrow_L (tl, tr) -> TArrow (unzip tl, tr)
    | Prod_L (tl, tr) -> TProd (unzip tl, tr)
    | Arrow_R (tl, tr) -> TArrow (tl, unzip tr)
    | Prod_R (tl, tr) -> TProd (tl, unzip tr)
    | List_L tl -> TList (unzip tl)
  in
  { id; node }
