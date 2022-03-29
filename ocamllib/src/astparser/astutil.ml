open Ast

exception SyntaxError of string
exception NotImplemented

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
let rec eval (e : Expr.t) (stack : int) : Value.t =
  let eval_unop (op: Expr.unop) (v: Value.t): Value.t =
    match op with
    | OpNeg ->
        let n = expecting_num v in
        VInt (-1 * n)
  in
  if stack = 0 then 
    raise (Failure "Stack overflow")
  else
    match e with
      | EInt n -> VInt n
      | EBool b -> VBool b
      | EFun (x, e_body) -> VFun (x, e_body)
      | ENil -> VNil
      | EUnOp (unop, e) -> eval_unop unop (eval e stack)
      | EBinOp (e_l, binop, e_r) -> eval_binop (eval e_l stack) binop (eval e_r stack) stack
      | EIf (e_cond, e_then, e_else) ->
        let b = expecting_bool (eval e_cond stack) in
        if b then eval e_then stack else eval e_else stack
      | ELet (x, e_def, e_body) ->
        let v_def = eval e_def stack in
        eval (subst (Value.to_expr v_def) x e_body) stack
      | EPair (e_l, e_r) -> VPair (eval e_l stack, eval e_r stack)
      | EFix (x, e_body) ->
        let unrolled = subst (EFix (x, e_body)) x e_body in
        eval unrolled (stack - 1)
      | _ -> raise (Failure "Invalid syntax")
and eval_binop (v_l: Value.t) (op: Expr.binop) (v_r: Value.t) (stack : int) : Value.t =
  match op with
  | OpAp ->
    let (x, body) = expecting_fun v_l in
    eval (subst (Value.to_expr v_r) x body) stack
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
let rec c_to_expr (nodes : node list) (edges : edge list) (root : int) : Expr.t =
  let tag = List.nth nodes root in
  if tag >= 30 then Expr.tag_to_node tag None None None else
  let adj_nodes = get_adj_nodes edges root in
  let get_nth_child n = 
    let nth_child = List.filter (fun (_, _, child_num) -> child_num = n) adj_nodes in
    match nth_child with
    | [] -> None
    | [(_, endpt, _)] -> Some (c_to_expr nodes edges endpt)
    | _ -> raise (Failure "Invalid syntax")
  in
  Expr.tag_to_node tag (get_nth_child 1) (get_nth_child 2) (get_nth_child 3)

(* Change tree representation to list representation of AST *)
let expr_to_c (e : Expr.z_t) : (graph * int * int list) = 
  let add_node (nodes : node list) (tag : Expr.tag) : (node list * int) =
    let new_nodes = nodes @ [tag] in (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : (edge list) =
    new_edge :: edges
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
  let rec ast_to_c (e : Expr.t) (nodes : node list) (edges : edge list) (vars : varlist) : (graph * int * varlist) = 
    let add_subtree (e: Expr.t) (nodes : node list) (edges : edge list) (vars : varlist) (root : int) (num_child : int) : graph =
      let ((nodes, edges), new_root, _) = ast_to_c e nodes edges vars in
      let edges = add_edge edges (root, new_root, num_child) in
      (nodes, edges)
    in
    let tag = Expr.node_to_tag e in
    let (nodes, root) = add_node nodes tag in
    match e with
      | EVar _ -> 
        let edges = add_edge edges (find_var (get_var_name e) vars, root, -1) in 
        ((nodes, edges), root, vars)
      | EInt _ | EBool _ | EHole | ENil -> 
        ((nodes, edges), root, vars)
      | EUnOp (_, e) -> 
        (add_subtree e nodes edges vars root 1, root, vars)
      | EBinOp (e1, _, e2) -> 
        let (nodes, edges) = add_subtree e1 nodes edges vars root 1 in
        (add_subtree e2 nodes edges vars root 2, root, vars)
      | ELet (var, edef, ebody) ->
        let (nodes, new_root) = add_node nodes (Expr.node_to_tag (EVar var)) in
        let edges = add_edge edges (root, new_root, 1) in
        let (nodes, edges) = add_subtree edef nodes edges vars root 2 in
        let vars = add_var var new_root vars in
        (add_subtree ebody nodes edges vars root 3, root, vars)
      | EIf (econd, ethen, eelse) ->
        let (nodes, edges) = add_subtree econd nodes edges vars root 1 in
        let (nodes, edges) = add_subtree ethen nodes edges vars root 2 in
        (add_subtree eelse nodes edges vars root 3, root, vars)
      | EFun (var, e) | EFix (var, e) ->
        let (nodes, new_root) = add_node nodes (Expr.node_to_tag (EVar var)) in
        let edges = add_edge edges (root, new_root, 1) in
        let vars = add_var var new_root vars in
        (add_subtree e nodes edges vars root 2, root, vars)
      | EPair (e1, e2) ->
        let (nodes, edges) = add_subtree e1 nodes edges vars root 1 in
        (add_subtree e2 nodes edges vars root 2, root, vars)
  in
  let rec find_cursor (e : Expr.z_t) (vars : varlist) (index : int) : int * varlist = 
    let rec extend (vars : varlist) (new_var : Var.t) (index : int) = 
      match vars with
        | [] -> [(new_var, index)]
        | hd :: tl -> 
          let (hd_var, _) = hd in 
          if Var.equal hd_var new_var then (new_var, index) :: tl else hd :: (extend tl new_var index)
    in
    match e with
      | Cursor _ -> (index, vars)
      | EUnOp_L (_, e) -> find_cursor e vars (index + 1)
      | EBinOp_L (e1, _, _) -> find_cursor e1 vars (index + 1)
      | EBinOp_R (e1, _, e2) -> find_cursor e2 vars (index + 1 + Expr.size e1)
      | ELet_L (_, e1, _) -> find_cursor e1 vars (index + 2)
      | ELet_R (x, e1, e2) -> 
        let vars = extend vars x (index + 1) in
        find_cursor e2 vars (index + 2 + Expr.size e1)
      | EIf_L (econd, _, _) -> find_cursor econd vars (index + 1)
      | EIf_C (econd, ethen, _) -> find_cursor ethen vars (index + 1 + Expr.size econd)
      | EIf_R (econd, ethen, eelse) -> find_cursor eelse vars (index + 1 + Expr.size econd + Expr.size ethen)
      | EFun_L (x, e) ->
        let vars = extend vars x (index + 1) in
        find_cursor e vars (index + 2)
      | EFix_L (x, e) -> 
        let vars = extend vars x (index + 1) in
        find_cursor e vars (index + 2)
      | EPair_L (e1, _) -> find_cursor e1 vars (index + 1)
      | EPair_R (e1, e2) -> find_cursor e2 vars (index + 1 + Expr.size e1)
  in
    let (graph, _, _) = ast_to_c (unzip_ast e) [] [] [] in
    let (cursor, vars) = find_cursor e [] 0 in 
    let get_index = List.map (fun (_, index) -> index) in
    (graph, cursor, get_index vars)

let rec nodelist_to_words (nodes : node list) : string list = 
  match nodes with
    | [] -> []
    | hd :: tl -> (Expr.tag_to_word hd) :: nodelist_to_words tl

let select_root : (Expr.t -> Expr.z_t) =
  (* Convert an unzipped ast into a zipped one, by selecting the root*)
  (function tree -> Expr.Cursor tree) 

let serialize (zast : Expr.z_t) : string = 
  Core.Sexp.to_string (Expr.sexp_of_z_t zast)

let deserialize (zast : string) : Expr.z_t = 
  Expr.z_t_of_sexp (Core.Sexp.of_string zast)