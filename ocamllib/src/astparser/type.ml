exception SyntaxError of string
exception RuntimeError of string
exception TranslationError of string
exception NotImplemented

(* Basic types *)
module Typ = struct
  type t = 
  | TInt 
  | TBool 
  | TArrow of t * t 
  | TProd of t * t 
  | THole
  | TList of t  (* temporarily include lists *)
  [@@deriving sexp]

  type z_t = 
  | Cursor of  t
  | Arrow_L of z_t * t 
  | Arrow_R of t * z_t 
  | Prod_L of z_t * t
  | Prod_R of t* z_t 
  | List_L of z_t 

  (* Check if two types are equal *)
  let rec equal (ty : t) (ty' : t) : bool =
    match (ty, ty') with
    | TInt, TInt 
    | TBool, TBool 
    | THole, THole -> true
    | TArrow (tin1, tout1), TArrow (tin2, tout2) ->
        equal tin1 tin2 && equal tout1 tout2
    | TList t_1 , TList t_2 ->  equal t_1 t_2 
    | _ -> false

  let node_to_tag (node : t) : int =
    match node with    (* HOW ARE THESE NUMBERS CHOSEN*)
    | TInt -> 20
    | TBool -> 21
    | TArrow (_, _) -> 22
    | TProd (_, _) -> 23
    | TList _ -> 24
    | THole -> 25

  let tag_to_node (tag : int) : t =
    match tag with
    | 20 -> TInt
    | 21 -> TBool
    | 22 -> TArrow (THole, THole)
    | 23 -> TProd (THole, THole)
    | 24 -> TList  (THole)
    | 25 -> THole
    | _ -> raise (SyntaxError "Unrecognized type")

  type edge = int * int * int
  type node = int
  type graph = node list * edge list

  let rec from_list (nodes : node list) (edges : edge list) (root : int) : t =
    let get_adj_nodes (edges : edge list) (start_node : int) : edge list =
      List.filter (fun (start, _, _) -> start = start_node) edges
    in
    let get_nth_child (edges : edge list) (n : int) : int =
      let child = List.filter (fun (_, _, idx) -> idx = n) edges in
      match child with
      | [] -> raise (TranslationError "No child when there should be")
      | [ (_, stop, _) ] -> stop
      | _ -> raise (TranslationError "More than one child at this position")
    in
    let tag = List.nth nodes root in
    let new_node = tag_to_node tag in
    match new_node with
    | TInt -> TInt
    | TBool -> TBool
    | TArrow (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        TArrow
          ( from_list nodes edges (get_nth_child adj_nodes 1),
            from_list nodes edges (get_nth_child adj_nodes 2) )
    | TProd (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        TProd
          ( from_list nodes edges (get_nth_child adj_nodes 1),
            from_list nodes edges (get_nth_child adj_nodes 2) )
    | TList _ -> 
        let adj_nodes = get_adj_nodes edges root in 
        TList (from_list nodes edges (get_nth_child adj_nodes 1))
    | THole -> THole

  let to_list (tree : t) : graph * int =
    let add_node (nodes : node list) (tag : int) : node list * int =
      let new_nodes = nodes @ [ tag ] in
      (new_nodes, List.length nodes)
    in
    let add_edge (edges : edge list) (new_edge : edge) : edge list =
      new_edge :: edges
    in
    let rec to_list_aux (tree : t) (nodes : node list) (edges : edge list) :
        graph * int =
      let add_subtree (e : t) (nodes : node list) (edges : edge list)
          (root : int) (num_child : int) : graph =
        let (nodes, edges), new_root = to_list_aux e nodes edges in
        let edges = add_edge edges (root, new_root, num_child) in
        (nodes, edges)
      in
      let tag = node_to_tag tree in
      let nodes, root = add_node nodes tag in
      match tree with
      | TInt | TBool | THole -> ((nodes, edges), root)
      | TList t1 ->  ((add_subtree t1 nodes edges root 1), root)
      | TArrow (t1, t2) | TProd (t1, t2) ->
          let nodes, edges = add_subtree t1 nodes edges root 1 in
          (add_subtree t2 nodes edges root 2, root)
    in
    to_list_aux tree [] []
end
