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
  | TUnit
(* temporarily include lists *)

and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

type p_t =
  | Int
  | Bool
  | Arrow of p_t * p_t
  | Prod of p_t * p_t
  | Hole
  | List of p_t
  | Unit
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
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

let make_node (node : node) : t = { id = Id.generate (); node; starter = false }

let make_z_node (node : z_node) : z_t =
  { id = Id.generate (); node; starter = false }

let make_dummy_node (node : node) : t = { id = -1; node; starter = false }

let rec strip (e : t) : p_t =
  match e.node with
  | TInt -> Int
  | TBool -> Bool
  | TArrow (t1, t2) -> Arrow (strip t1, strip t2)
  | TProd (t1, t2) -> Prod (strip t1, strip t2)
  | THole -> Hole
  | TList t -> List (strip t)
  | TUnit -> Unit

let rec add_metadata (e : p_t) : t =
  match e with
  | Int -> make_node TInt
  | Bool -> make_node TBool
  | Arrow (t1, t2) -> make_node (TArrow (add_metadata t1, add_metadata t2))
  | Prod (t1, t2) -> make_node (TProd (add_metadata t1, add_metadata t2))
  | Hole -> make_node THole
  | List t -> make_node (TList (add_metadata t))
  | Unit -> make_node TUnit

(* Check if two types are equal *)
let rec equal (ty : t) (ty' : t) : bool =
  match (ty.node, ty'.node) with
  | TInt, TInt | TBool, TBool | THole, THole | TUnit, TUnit -> true
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
let rec consistent (ty : p_t) (ty' : p_t) : bool =
  match (ty, ty') with
  | _, Hole | Hole, _ -> true
  | Int, Int | Bool, Bool | Unit, Unit -> true
  | Arrow (t1, t2), Arrow (t1', t2') | Prod (t1, t2), Prod (t1', t2') ->
      consistent t1 t1' && consistent t2 t2'
  | List t_1, List t_2 -> consistent t_1 t_2
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
  | TInt | TBool | THole | TUnit -> 1
  | TList t1 -> 1 + size t1
  | TArrow (t1, t2) | TProd (t1, t2) -> 1 + size t1 + size t2

let%test_module "Test Typ.size" =
  (module struct
    let check t n = size (add_metadata t) = n

    let%test _ = check (Arrow (Prod (Int, Int), Bool)) 5
    let%test _ = check (Arrow (Arrow (Int, Int), Prod (Bool, Hole))) 7
  end)

let select_root (e : t) : z_t =
  { id = e.id; node = Cursor e.node; starter = e.starter }

let rec unzip (tree : z_t) : t =
  let node : node =
    match tree.node with
    | Cursor subtree -> subtree
    | Arrow_L (tl, tr) -> TArrow (unzip tl, tr)
    | Prod_L (tl, tr) -> TProd (unzip tl, tr)
    | Arrow_R (tl, tr) -> TArrow (tl, unzip tr)
    | Prod_R (tl, tr) -> TProd (tl, unzip tr)
    | List_L tl -> TList (unzip tl)
  in
  { id = tree.id; node; starter = tree.starter }

let rec set_starter (typ : t) (b : bool) : t =
  let new_node =
    match typ.node with
    | TInt | TBool | THole | TUnit -> typ.node
    | TArrow (t1, t2) -> TArrow (set_starter t1 b, set_starter t2 b)
    | TProd (t1, t2) -> TProd (set_starter t1 b, set_starter t2 b)
    | TList t -> TList (set_starter t b)
  in
  { typ with node = new_node; starter = b }
