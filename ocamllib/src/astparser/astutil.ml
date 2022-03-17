open Ast

exception SyntaxError of string
exception NotImplemented

(* Substitute the variable x in e2 with e1 (i.e. [e1/x]e2) *)
let rec subst (e1 : Expr.t) (x : Var.t) (e2 : Expr.t) : Expr.t =
  let subx = subst e1 x in
  let subx_unless cond = if cond then Fun.id else subx in
  match e2 with
    | EBool _ | EInt _ | ENil | EHole -> e2
    | EVar y -> if Var.equal x y then e1 else e2
    | EUnOp (op, e) -> EUnOp (op, subx e)
    | EBinOp (e_l, op, e_r) -> EBinOp (subx e_l, op, subx e_r)
    | EIf (e_cond, e_then, e_else) -> EIf (subx e_cond, subx e_then, subx e_else)
    | EFun (y, e_body) -> EFun (y, subx_unless (Var.equal x y) e_body)
    | ELet (y, e_def, e_body) -> ELet (y, subx e_def, subx_unless (Var.equal x y) e_body)
    | EFix (y, e_body) -> EFix (y, subx_unless (Var.equal x y) e_body)
    | EPair (e_l, e_r) -> EPair (subx e_l, subx e_r)


let expecting_num (v : Value.t): int =
  match v with
  | VInt n -> n
  | _ -> raise (Failure "Cannot be evaluated")

let expecting_bool (v : Value.t): bool =
  match v with
  | VBool b -> b
  | _ -> raise (Failure "Cannot be evaluated")

let expecting_fun (v : Value.t): (Var.t * Expr.t) =
  match v with
  | VFun (x, body) -> (x, body)
  | _ -> raise (Failure "Cannot be evaluated")  

(* Evalutate the expression e *)
let rec eval (e : Expr.t) : Value.t =
  let eval_unop (op: Expr.unop) (v: Value.t): Value.t =
    match op with
    | OpNeg ->
        let n = expecting_num v in
        VInt (-1 * n)
  in
  match e with
    | EInt n -> VInt n
    | EBool b -> VBool b
    | EFun (x, e_body) -> VFun (x, e_body)
    | ENil -> VNil
    | EUnOp (unop, e) -> eval_unop unop (eval e)
    | EBinOp (e_l, binop, e_r) -> eval_binop (eval e_l) binop (eval e_r)
    | EIf (e_cond, e_then, e_else) ->
      let b = expecting_bool (eval e_cond) in
      if b then eval e_then else eval e_else
    | ELet (x, e_def, e_body) ->
      let v_def = eval e_def in
      eval (subst (Value.to_expr v_def) x e_body)
    | EPair (e_l, e_r) -> VPair (eval e_l, eval e_r)
    | EFix (x, e_body) ->
      let unrolled = subst (EFix (x, e_body)) x e_body in
      eval unrolled
    | _ -> raise (Failure "Invalid syntax")
and eval_binop (v_l: Value.t) (op: Expr.binop) (v_r: Value.t): Value.t =
  match op with
  | OpAp ->
    let (x, body) = expecting_fun v_l in
    eval (subst (Value.to_expr v_r) x body)
  | OpPlus | OpMinus | OpTimes | OpDiv ->
    let f =
      match op with
      | OpPlus -> (+)
      | OpMinus -> (-)
      | OpTimes -> ( * )
      | _ -> (/)
    in
    VInt (f (expecting_num v_l) (expecting_num v_r))
  | OpLt | OpLe | OpGt | OpGe | OpEq | OpNe ->
    let f =
      match op with
      | OpLt -> (<)
      | OpLe -> (<=)
      | OpGt -> (>)
      | OpGe -> (>=)
      | OpNe -> (!=)
      | _ -> (=)
    in
    VBool (f (expecting_num v_l) (expecting_num v_r))
  | OpCon ->
    raise NotImplemented
  
(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

(* Parse a file (assuming it is a well-typed .ml file) into an ast *)
let parse_file filename =
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let s = read_whole_file filename in
  parse s

type edge = (int * int * int)
type node = int
type graph = (node list) * (edge list)
type varlist = (Var.t * int) list

let get_adj_nodes (edges : edge list) (start_node : int) : edge list =
  List.filter (fun (start, _, _) -> start = start_node) edges

(* Change list representation to tree representation of AST *)
let rec c_to_expr (nodes : node list) (edges : edge list) (curr_node : int) : Expr.t =
  let tag = List.nth nodes curr_node in
  if tag >= 30 then Expr.tag_to_node tag None None None else
  let adj_nodes = get_adj_nodes edges curr_node in
  let get_nth_child n = 
    let nth_child = List.filter (fun (_, _, child_num) -> child_num = n) adj_nodes in
    match nth_child with
    | [] -> None
    | [(_, endpt, _)] -> Some (c_to_expr nodes edges endpt)
    | _ -> raise (Failure "Invalid syntax")
  in
  Expr.tag_to_node tag (get_nth_child 1) (get_nth_child 2) (get_nth_child 3)

(* Change tree representation to list representation of AST *)
let expr_to_c (e : Expr.t) : (graph * int) = 
  let add_node (nodes : node list) (tag : Expr.tag) : (node list * int) =
    let new_nodes = nodes @ [tag] in (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : (edge list) =
    new_edge :: edges
  in
  let is_var (tag : Expr.tag) : bool =
    (tag >= 38) && (tag <= 40)
  in
  let get_var_name (v : Expr.t) : string =
    match v with
      | EVar s -> s
      | _ -> raise (SyntaxError "")
  in
  let add_var (var : Var.t) (index : int) (vars : varlist) : varlist = 
    (var, index) :: vars
  in
  let find_var (target : string) (vars : varlist) : int =
    let indices = List.filter (fun (var, index) -> Var.equal target var) vars in
    match indices with
      | (_, index) :: tl -> index
      | [] -> raise (SyntaxError "Expression not closed")
  in
  let is_binding (tag : Expr.tag) : bool = 
    (tag = 15) || (tag = 16) || (tag = 13)
  in
  let rec expr_to_c_aux (e : Expr.t) (nodes : node list) (edges : edge list) (vars : varlist) : (graph * int) = 
    let tag = Expr.node_to_tag e in
    if is_var tag then
      let (nodes, root) = add_node nodes tag in 
      let edges = add_edge edges (find_var (get_var_name e) vars, root, -1) in 
      ((nodes, edges), root)
    else if tag >= 30 then 
      let (nodes, root) = add_node nodes tag in 
      ((nodes, edges), root)
    else 
      let add_unary subexpr = 
        let (nodes, new_root) = add_node nodes tag in
        let ((nodes, edges), root) = expr_to_c_aux subexpr nodes edges vars in
        let edges = add_edge edges (new_root, root, 1) in
        ((nodes, edges), new_root)
      in
      let add_binary sub1 sub2 = 
        let (nodes, new_root) = add_node nodes tag in
        let ((nodes, edges), root1) = 
          if is_binding tag then
            let (nodes, root) = add_node nodes (Expr.node_to_tag sub1) in
            ((nodes, edges), root)
          else
            expr_to_c_aux sub1 nodes edges vars 
        in
        let vars = if is_binding tag then add_var (get_var_name sub1) root1 vars else vars in
        let ((nodes, edges), root2) = expr_to_c_aux sub2 nodes edges vars in
        let edges = add_edge edges (new_root, root1, 1) in
        let edges = add_edge edges (new_root, root2, 2) in
        ((nodes, edges), new_root)
      in
      let add_ternary sub1 sub2 sub3 = 
        let (nodes, new_root) = add_node nodes tag in
        let ((nodes, edges), root1) = 
          if is_binding tag then
            let (nodes, root) = add_node nodes (Expr.node_to_tag sub1) in
            ((nodes, edges), root)
          else
            expr_to_c_aux sub1 nodes edges vars 
        in
        let ((nodes, edges), root2) = expr_to_c_aux sub2 nodes edges vars in
        let vars = if is_binding tag then add_var (get_var_name sub1) root1 vars else vars in
        let ((nodes, edges), root3) = expr_to_c_aux sub3 nodes edges vars in
        let edges = add_edge edges (new_root, root1, 1) in
        let edges = add_edge edges (new_root, root2, 2) in
        let edges = add_edge edges (new_root, root3, 3) in
        ((nodes, edges), new_root)
      in
      match e with
        | EUnOp (_, subexpr) -> add_unary subexpr
        | EBinOp (sub1, _ , sub2) -> add_binary sub1 sub2
        | ELet (sub1, sub2, sub3) -> add_ternary (EVar sub1) sub2 sub3
        | EIf (sub1, sub2, sub3) -> add_ternary sub1 sub2 sub3
        | EFun (sub1, sub2) -> add_binary (EVar sub1) sub2
        | EFix (sub1, sub2) -> add_binary (EVar sub1) sub2
        | EPair (sub1, sub2) -> add_binary sub1 sub2
        | _ -> raise (Failure "Incorrect syntax")
    in
      expr_to_c_aux e [] [] []

let rec nodelist_to_words (nodes : node list) : string list = 
  match nodes with
    | [] -> []
    | hd :: tl -> (Expr.tag_to_word hd) :: nodelist_to_words tl

let select_root : (Expr.t -> Expr.z_t) =
  (* Convert an unzipped ast into a zipped one, by selecting the root*)
  (function tree -> Expr.Cursor tree) 

let rec unzip_ast  (tree : Expr.z_t) : Expr.t =
  match tree with 
  | Cursor arg -> arg 
  | EUnOp_L (unop, l_child) -> EUnOp (unop, unzip_ast l_child)
  | EBinOp_L (l_child, binop,r_child) -> EBinOp (unzip_ast l_child, binop,r_child)
  | EBinOp_R (l_child, binop,r_child) -> EBinOp (l_child, binop,unzip_ast r_child)
  | ELet_L (var,l_child, r_child) -> ELet (var, unzip_ast l_child, r_child)
  | ELet_R (var,l_child, r_child) -> ELet (var, l_child, unzip_ast r_child)
  | EIf_L (l, c, r) -> EIf(unzip_ast l, c,r)
  | EIf_C (l, c, r) -> EIf(l, unzip_ast c, r) 
  | EIf_R (l, c, r) -> EIf(l, c, unzip_ast r) 
  | EPair_L (l, r) ->  EPair (unzip_ast l, r) 
  | EPair_R (l, r) ->  EPair (l, unzip_ast r)
  | EFun_L (var, child) -> EFun(var, unzip_ast child) 
  | EFix_L (var, child) -> EFix(var, unzip_ast child)