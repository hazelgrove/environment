(* Converts types to ints/lists *)
exception SyntaxError of string

open Type

let node_list =
  [
    TInt;
    TBool;
    TArrow (make_dummy_node THole, make_dummy_node THole);
    TProd (make_dummy_node THole, make_dummy_node THole);
    TList (make_dummy_node THole);
    THole;
  ]

let num_nodes = List.length node_list

let node_list_equal (e1 : node) (e2 : node) : bool =
  if e1 = e2
  then true
  else
    match (e1, e2) with
    | TArrow _, TArrow _ | TProd _, TProd _ | TList _, TList _ -> true
    | _ -> false

let node_to_tag (ty : t) : int =
  let rec find_node x lst c =
    match lst with
    | [] -> raise (Failure "Invalid node")
    | hd :: tl -> if node_list_equal x hd then c else find_node x tl (c + 1)
  in
  find_node ty.node node_list 0

let tag_to_node (tag : int) : t =
  let node =
    try List.nth node_list tag
    with Failure _ | Invalid_argument _ ->
      raise (Failure "Invalid node index")
  in
  make_node node

(* Shorthands for the following functions *)
type edge = int * int * int
type graph = int list * edge list

let rec from_list ~(nodes : int list) ~(edges : edge list) ~(root : int) : t =
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
  let node = tag_to_node tag in
  let new_node =
    match node.node with
    | TInt -> TInt
    | TBool -> TBool
    | TArrow (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        TArrow
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | TProd (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        TProd
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | TList _ ->
        let adj_nodes = get_adj_nodes edges root in
        TList (from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1))
    | THole -> THole
  in
  { id = node.id; node = new_node }

(* let%test_module "Test TypeConv.from_list" =
   (module struct
     let%test _ = from_list ~nodes:[ 20 ] ~edges:[] ~root:0 = TInt

     let%test _ =
       from_list ~nodes:[ 23; 22; 20; 21; 25 ]
         ~edges:[ (0, 1, 1); (0, 4, 2); (1, 2, 1); (1, 3, 2) ]
         ~root:0
       = TProd (TArrow (TInt, TBool), THole)
   end) *)

let to_list (tree : t) : graph * int =
  let add_node (nodes : int list) (tag : int) : int list * int =
    let new_nodes = nodes @ [ tag ] in
    (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : edge list =
    new_edge :: edges
  in
  let rec to_list_aux (tree : t) (nodes : int list) (edges : edge list) :
      graph * int =
    let add_subtree (e : t) (nodes : int list) (edges : edge list) (root : int)
        (num_child : int) : graph =
      let (nodes, edges), new_root = to_list_aux e nodes edges in
      let edges = add_edge edges (root, new_root, num_child) in
      (nodes, edges)
    in
    let tag = node_to_tag tree in
    let nodes, root = add_node nodes tag in
    match tree.node with
    | TInt | TBool | THole -> ((nodes, edges), root)
    | TList t1 -> (add_subtree t1 nodes edges root 1, root)
    | TArrow (t1, t2) | TProd (t1, t2) ->
        let nodes, edges = add_subtree t1 nodes edges root 1 in
        (add_subtree t2 nodes edges root 2, root)
  in
  to_list_aux tree [] []

(* let%test_module "Test TypeConv.to_list" =
   (module struct
     let check_id tree =
       let (nodes, edges), root = to_list tree in
       let changed_tree = from_list ~nodes ~edges ~root in
       tree = changed_tree

     let%test _ = check_id TInt
     let%test _ = check_id (TProd (TArrow (TInt, TBool), THole))
   end) *)

let rec to_string (tree : p_t) : string =
  match tree with
  | Int -> "int "
  | Bool -> "bool "
  | Arrow (t1, t2) -> to_string t1 ^ "-> " ^ to_string t2
  | Prod (t1, t2) -> to_string t1 ^ "* " ^ to_string t2
  | List t1 -> to_string t1 ^ " list"
  | Hole -> "? "
