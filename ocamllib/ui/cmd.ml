open Astlib
open Ast
open Astutil

let rec print_tabs (num_tabs : int) : unit =
  if num_tabs > 0
  then (
    print_string "  ";
    print_tabs (num_tabs - 1))
  else ()

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
        | OpCon -> ":: "
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
  | _ -> print_endline "Not supported yet."

let get_action : unit -> Action.t option =
 fun _ ->
  let act_string = read_line () in
  match act_string with
  | _ ->
      print_endline "Invalid action.";
      None

let rec interactive_loop (ast : Expr.z_t) (action : Action.t option) : unit =
  let ast = match action with Some act -> change_ast ast act | None -> ast in
  print_zast ast 0;
  let next_action = get_action () in
  interactive_loop ast next_action

let () =
  interactive_loop
    (Cursor (parse "if true then (fun x -> x + 1) else (fun x -> 2 * x)"))
    None
