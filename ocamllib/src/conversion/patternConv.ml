open Pattern

exception TranslationError of string

let rec to_string (p : p_t) : string =
  match p with
  | Const c -> ConstConv.to_string c
  | Var v -> "x" ^ string_of_int v
  | Cons (p1, p2) -> to_string p1 ^ " :: " ^ to_string p2
  | Wild -> "_"

let node_list =
  [
    PConst (Int (-2));
    PConst (Int (-1));
    PConst (Int 0);
    PConst (Int 1);
    PConst (Int 2);
    PConst (Bool true);
    PConst (Bool false);
    PConst Nil;
    PCons (make_dummy_node PWild, make_dummy_node PWild);
    PWild;
  ]
  @ List.init Var.max_num_vars (fun i -> PVar i)

let num_nodes = List.length node_list

let node_list_equal (p1 : node) (p2 : node) : bool =
  if p1 = p2
  then true
  else match (p1, p2) with PCons _, PCons _ -> true | _ -> false

let node_to_tag (p : t) : int =
  let rec find_node x lst c =
    match lst with
    | [] -> raise (Failure "Invalid node")
    | hd :: tl -> if node_list_equal x hd then c else find_node x tl (c + 1)
  in
  find_node p.node node_list TypeConv.num_nodes

let tag_to_node (tag : int) : t =
  let tag = tag - TypeConv.num_nodes in
  let node =
    try List.nth node_list tag
    with Failure _ | Invalid_argument _ ->
      raise (Failure ("Invalid node index " ^ string_of_int tag))
  in
  make_node node

type edge = int * int * int
type graph = int list * edge list
type varlist = (Var.t * int) list

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
    | PConst c -> PConst c
    | PVar v -> PVar v
    | PCons (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        PCons
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | PWild -> PWild
  in
  { node with node = new_node }

let to_list (p : t) : graph * int * varlist =
  let add_node (nodes : int list) (tag : int) : int list * int =
    let new_nodes = nodes @ [ tag ] in
    (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : edge list =
    new_edge :: edges
  in
  let add_var (var : Var.t) (index : int) (vars : varlist) : varlist =
    (var, index) :: vars
  in
  let rec to_list_aux (p : t) (nodes : int list) (edges : edge list)
      (vars : varlist) : graph * int * varlist =
    let add_subtree (p : t) (nodes : int list) (edges : edge list) (root : int)
        (vars : varlist) (num_child : int) : graph * varlist =
      let (nodes, edges), new_root, vars = to_list_aux p nodes edges vars in
      let edges = add_edge edges (root, new_root, num_child) in
      ((nodes, edges), vars)
    in
    let tag = node_to_tag p in
    let nodes, root = add_node nodes tag in
    match p.node with
    | PConst _ | PWild -> ((nodes, edges), root, vars)
    | PVar x -> ((nodes, edges), root, add_var x root vars)
    | PCons (p1, p2) ->
        let (nodes, edges), vars = add_subtree p1 nodes edges root vars 1 in
        let (nodes, edges), vars = add_subtree p2 nodes edges root vars 2 in
        ((nodes, edges), root, vars)
  in
  to_list_aux p [] [] []

let rec get_starter_list (p : t) : bool list =
  match p.node with
  | PConst _ | PVar _ | PWild -> [ p.starter ]
  | PCons (p1, p2) -> p.starter :: (get_starter_list p1 @ get_starter_list p2)
