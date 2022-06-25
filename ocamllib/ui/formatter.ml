let print_tabs (num_tabs : int) : unit =
  print_string (String.make (2 * num_tabs) ' ')

let rec print_ast (ast : Expr.t) (num_tabs : int) : unit =
  print_tabs num_tabs;
  match ast with
  | EVar x ->
      print_string x;
      print_string " "
  | EInt n ->
      print_int n;
      print_string " "
  | EBool true -> print_string "true "
  | EBool false -> print_string "false "
  | EUnOp (op, e) ->
      print_string "( ";
      let op_string = match op with OpNeg -> "-" in
      print_string op_string;
      print_ast e 0;
      print_string ") "
  | EBinOp (e1, op, e2) ->
      print_string "( ";
      print_ast e1 0;
      let op_string =
        match op with
        | OpPlus -> "+ "
        | OpMinus -> "- "
        | OpTimes -> "* "
        | OpDiv -> "/ "
        | OpLt -> "< "
        | OpLe -> "<= "
        | OpGt -> "> "
        | OpGe -> ">= "
        | OpEq -> "= "
        | OpNe -> "!= "
        | OpCons -> ":: "
        | OpAp -> " "
      in
      print_string op_string;
      print_ast e2 0;
      print_string ") "
  | ELet (x, edef, ebody) ->
      print_string "let ";
      print_string x;
      print_endline " = ";
      print_ast edef (num_tabs + 1);
      print_endline "";
      print_tabs num_tabs;
      print_endline "in";
      print_ast ebody num_tabs
  | EIf (econd, ethen, eelse) ->
      print_endline "if";
      print_ast econd (num_tabs + 1);
      print_endline "";
      print_endline "then";
      print_ast ethen (num_tabs + 1);
      print_endline "";
      print_endline "else";
      print_ast eelse (num_tabs + 1)
  | EFun (arg, _, e) ->
      print_string "fun ";
      print_string arg;
      print_endline " ->";
      print_ast e (num_tabs + 1)
  | EFix (x, _, e) ->
      print_string "fix ";
      print_string x;
      print_endline " ->";
      print_ast e (num_tabs + 1)
  | EPair (e1, e2) ->
      print_string "(";
      print_ast e1 0;
      print_string ", ";
      print_ast e2 0;
      print_string ") "
  | EHole -> print_string "HOLE "
  | _ -> print_endline "Not supported yet."

let print_zast (ast : Expr.z_t) (num_tabs : int) : unit =
  match ast with
  | Cursor tree ->
      print_string "$";
      print_ast tree num_tabs
  | _ -> print_endline "ZAst Not supported yet."
