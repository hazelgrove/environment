exception SyntaxError of string
exception RuntimeError of string
exception TranslationError of string
exception NotImplemented

(* Basic types *)
module Typ = struct
  type t = TInt | TBool | TArrow of t * t | TProd of t * t | THole
  [@@deriving sexp]

  (* Check if two types are equal *)
  let rec equal (ty : t) (ty' : t) : bool =
    match (ty, ty') with
    | TInt, TInt | TBool, TBool | THole, THole -> true
    | TArrow (tin1, tout1), TArrow (tin2, tout2) ->
        equal tin1 tin2 && equal tout1 tout2
    | _ -> false

  (*
     Return the size of the Type Tree
     Input :
       - e : the Type Tree
     Output :
       - the size of the Type Tree
  *)
  let rec size (tree : t) : int =
    match tree with
    | TInt | TBool | THole -> 1
    | TArrow (t1, t2) | TProd (t1, t2) -> 1 + size t1 + size t2

  let%test_module "Test Typ.size" =
    (module struct
      let%test _ = size (TArrow (TProd (TInt, TInt), TBool)) = 5
      let%test _ = size (TArrow (TArrow (TInt, TInt), TProd (TBool, THole))) = 7
    end)

  let node_to_tag (node : t) : int =
    match node with
    | TInt -> 20
    | TBool -> 21
    | TArrow (_, _) -> 22
    | TProd (_, _) -> 23
    | THole -> 24

  let tag_to_node (tag : int) : t =
    match tag with
    | 20 -> TInt
    | 21 -> TBool
    | 22 -> TArrow (THole, THole)
    | 23 -> TProd (THole, THole)
    | 24 -> THole
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
    | THole -> THole

  let%test_module "Test Typ.from_list" =
    (module struct
      let%test _ = from_list [ 20 ] [] 0 = TInt

      let%test _ =
        from_list [ 23; 22; 20; 21; 24 ]
          [ (0, 1, 1); (0, 4, 2); (1, 2, 1); (1, 3, 2) ]
          0
        = TProd (TArrow (TInt, TBool), THole)
    end)

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
      | TArrow (t1, t2) | TProd (t1, t2) ->
          let nodes, edges = add_subtree t1 nodes edges root 1 in
          (add_subtree t2 nodes edges root 2, root)
    in
    to_list_aux tree [] []

  let%test_module "Test Typ.to_list" =
    (module struct
      let check_id tree =
        let (nodes, edges), root = to_list tree in
        let changed_tree = from_list nodes edges root in
        tree = changed_tree

      let%test _ = check_id TInt
      let%test _ = check_id (TProd (TArrow (TInt, TBool), THole))
    end)

  let rec to_string (tree : t) : string =
    match tree with
    | TInt -> "int "
    | TBool -> "bool "
    | TArrow (t1, t2) -> to_string t1 ^ "-> " ^ to_string t2
    | TProd (t1, t2) -> to_string t1 ^ "* " ^ to_string t2
    | THole -> "? "
end
