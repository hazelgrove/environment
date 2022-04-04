(* Include functions to be used by CInterface *)

open Ast
open Astutil

exception SyntaxError of string
exception IOError of string
exception NotImplemented of unit

type testType = (int * int)

(* 
    Given an zippered AST, apply the action 
    Input: 
      - e : an AST with cursor (TODO: implement zast)
      - action : action applied to e
    Output:
      the modified AST
*)
let change_ast (tree : Expr.z_t) (action : Action.t) : Expr.z_t =
  let rec act_on (tree : Expr.z_t) : Expr.z_t = 
    match action with 
      | Construct shape -> 
        begin match tree with 
          | EUnOp_L (op, r_child) -> EUnOp_L (op, act_on r_child) 
          | EBinOp_L (l_child, op, r_child) -> EBinOp_L (act_on l_child, op, r_child)
          | EBinOp_R (l_child, op, r_child) -> EBinOp_R (l_child, op, act_on r_child)
          | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
          | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)
          | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
          | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
          | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
          | EFun_L (var, typ, child) -> EFun_L (var, typ,act_on child)
          | EFix_L (var,typ , child) -> EFix_L (var, typ, act_on child)
          | EPair_L (l_child, r_child) -> EPair_L ( act_on l_child, r_child) 
          | EPair_R (l_child, r_child) -> EPair_R ( l_child, act_on r_child) 
          | Cursor subtree -> Cursor(
             match shape with 
            | Var varname -> EVar varname
            | Hole  -> EHole
            | Nil   -> ENil
            | Int  value -> EInt value      
            | Bool value  -> EBool value
            | UnOp op -> EUnOp(op, subtree)
            | BinOp_L op -> EBinOp(subtree, op, EHole )
            | BinOp_R op -> EBinOp(EHole, op, subtree)
            | Let_L varname  ->  ELet(varname,subtree,EHole)
            | Let_R varname ->  ELet(varname,EHole,subtree)
            | If_L     ->  EIf(subtree,EHole,EHole)
            | If_C     ->  EIf(EHole,subtree,EHole)
            | If_R     ->  EIf(EHole,EHole,subtree)
            | Fun (varname,typ)  -> EFun(varname,typ,subtree)
            | Fix (varname,typ)  -> EFix(varname,typ,subtree)
            | Pair_L       -> EPair(subtree,EHole)
            | Pair_R       -> EPair(EHole,subtree)
            ) 
        end
      | Move Child n -> 
        begin match tree with 
          | EUnOp_L (op,r_child) -> EUnOp_L (op,act_on r_child) 
          | EBinOp_L (l_child, op, r_child) -> EBinOp_L (act_on l_child, op, r_child)
          | EBinOp_R (l_child, op, r_child) -> EBinOp_R (l_child, op, act_on r_child)
          | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
          | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)
          | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
          | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
          | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)
          | EFun_L (var, typ, child) -> EFun_L (var, typ, act_on child)
          | EFix_L (var, typ, child) -> EFix_L (var, typ, act_on child)
          | EPair_L (l_child, r_child) -> EPair_L ( act_on l_child, r_child) 
          | EPair_R (l_child, r_child) -> EPair_R ( l_child, act_on r_child) 
          | Cursor subtree -> 
            begin match n with 
              | 0 -> (
                match subtree with
                  | EUnOp  (op,arg) -> EUnOp_L (op, Cursor (arg))
                  | EBinOp (arg_l, op, arg_r) -> EBinOp_L (Cursor (arg_l), op, arg_r)
                  | ELet (varn, arg_l, arg_r) -> ELet_L (varn, Cursor(arg_l),arg_r)
                  | EIf (arg_l, arg_c,arg_r) -> EIf_L (Cursor (arg_l), arg_c,arg_r)
                  | EFun (varname, typ, arg_l) -> EFun_L (varname, typ, Cursor (arg_l))
                  | EFix (varname, typ, arg_l) -> EFix_L (varname, typ, Cursor (arg_l))
                  | EPair (arg_l, arg_r) -> EPair_L (Cursor (arg_l),  arg_r)
                  | _ -> tree  (*all invalid actions are noops*)
                ) 
              | 1 ->( 
                match subtree with 
                | EBinOp (arg_l, op, arg_r) -> EBinOp_R (arg_l, op, Cursor (arg_r))
                | ELet (varn, arg_l, arg_r) -> ELet_R (varn, arg_l,Cursor(arg_r))
                | EIf (arg_l, arg_c,arg_r) -> EIf_C (arg_l, Cursor(arg_c),arg_r)
                | EPair (arg_l, arg_r) -> EPair_R (arg_l, Cursor (arg_r))
                | _ -> tree  (*all invalid actions are noops*)
                )
              | 2 -> (
                match subtree with 
                | EIf (arg_l, arg_c,arg_r) -> EIf_R (arg_l, arg_c,Cursor(arg_r))
                | _ -> tree  (*all invalid actions are noops*)
                )
              | _ -> tree
            end
          end
      | Move Parent -> 
        begin match tree with 
          | EUnOp_L (op, Cursor arg ) -> Cursor (EUnOp (op, arg))
          | EUnOp_L (op, arg) -> EUnOp_L (op, act_on arg) 

          | EBinOp_L (Cursor arg, op, r_child) -> Cursor (EBinOp (arg, op, r_child))
          | EBinOp_L (l_child, op, r_child) -> EBinOp_L (act_on l_child, op, r_child)
          | EBinOp_R (l_child, op, Cursor arg) -> Cursor (EBinOp (l_child, op, arg))
          | EBinOp_R (l_child, op, r_child) -> EBinOp_R (l_child, op, act_on r_child)

          (* new: *)
          | EPair_L (Cursor (l_child), r_child) -> Cursor ( EPair (l_child, r_child))
          | EPair_L (l_child, r_child)          -> EPair_L (act_on l_child, r_child )
          | EPair_R (l_child, Cursor(r_child))  -> Cursor (EPair (l_child, r_child))
          | EPair_R (l_child, r_child)          -> EPair_R (l_child, act_on r_child)
          
          | ELet_L (var,Cursor arg, r_child )  -> Cursor (ELet (var,arg, r_child))
          | ELet_L (var,l_child, r_child )  -> ELet_L (var,act_on l_child, r_child)
          | ELet_R (var,l_child, Cursor arg )  -> Cursor (ELet (var,l_child, arg))
          | ELet_R (var,l_child, r_child )  -> ELet_R (var,l_child,act_on r_child)

          | EIf_L (Cursor arg, c, r) -> Cursor (EIf (arg, c,r))
          | EIf_L (l, c, r) -> EIf_L (act_on l, c,r)  
          | EIf_C (l, Cursor arg, r) -> Cursor (EIf (l, arg, r))
          | EIf_C (l, c, r) -> EIf_C (l, act_on c, r)
          | EIf_R (l, c, Cursor arg) -> Cursor (EIf (l, c, arg))
          | EIf_R (l, c, r) -> EIf_R (l, c, act_on r)

          | EFun_L (var, typ, Cursor arg) ->  Cursor (EFun (var, typ, arg))
          | EFun_L (var, typ, child) -> EFun_L (var, typ, act_on child)
          | EFix_L (var, typ, Cursor arg) -> Cursor (EFun (var, typ, arg))
          | EFix_L (var, typ, child) -> EFix_L (var, typ, act_on child)
          | _ -> tree
        end
  in
  act_on tree

(* 
  Given a unit test set and AST, check if AST passes tests
  Input: 
    - test_set : a list of tests of testType with inputs and their corresponding output
    - code : the code to be evaluated upon
  Output:
    true, if code passes all tests
    false, otherwise
*)
let rec run_unit_tests (test_set : testType list) (code : Expr.t) : bool =
  let run_test (test: testType) (code : Expr.t) : bool =
    (* Assume code is a function in an ELet (_, EFun/EFix (_ , _), EHole) *)
    match code with
      | Expr.ELet (id, f, Expr.EHole) -> 
        begin match f with
          | EFun _ | EFix _ -> 
            let (test_input, test_output) = test in
            let output = eval (Expr.ELet (id, f, EBinOp(EVar id, Expr.OpAp, EInt test_input))) in
            begin match output with
              | VInt n -> n = test_output
              | _ -> false
            end
          | _ -> false
        end
      | _ -> false
  in
  match test_set with
    | [] -> true
    | hd :: tl -> if run_test hd code then run_unit_tests tl code else false
    
(* 
TODO: Comments on how this function works
TODO: Seems to have some bugs
*)
let possible_actions (expr: Expr.z_t ) : Action.avail_actions =( 
  let rec make_var_arr (i:int)  = 
    (* create an array of 10 falses *)
    if i <10 then false :: (make_var_arr (i+1) ) else [];
  in  

  let update_var_arr (varname:string) (varlist: bool list) : bool list  =(
    (* if a variable is in scope, mark its value to true *)
    let rec update_arr (i:int) (l: bool list): bool list = 
      match l with 
      | a ::tl -> ( ("v" ^ (string_of_int i) = varname) || a ) :: (update_arr (i+1) tl) 
      | []  -> []
    in update_arr 0 varlist
  ) in 

  let acts_init : Action.avail_actions  = 
    { move_parent = (match expr with | Cursor _ -> false | _-> true) ; 
      max_child = 0; 
      in_scope = make_var_arr 0 }
  in 
  (* now finally we recurse *)
  let rec recurse (expr:Expr.z_t) (state :Action.avail_actions):Action.avail_actions = 
    match expr with 
    | EUnOp_L (_, child) 
    | EBinOp_L (child,_,_) 
    | EBinOp_R (_, _, child) 
    | ELet_L (_,child,_)  (* variable not in self-scope in definition*)
    | EIf_L (child, _, _) 
    | EIf_C (_,child, _) 
    | EIf_R (_, _, child) 
    | EFix_L (_, child) 
    | EPair_L (child, _) 
    | EPair_R (_, child) -> recurse child state
    (*functions: update  *)
    | EFun_L (varname,_, child)
    | ELet_R (varname, _,child) -> 
      recurse child {move_parent=state.move_parent;
                     max_child=state.max_child; 
                     in_scope = update_var_arr varname state.in_scope}
    (*Now finally we do our cursor logic *)
    |Cursor subtree -> 
      {move_parent=state.move_parent;
       max_child =  (match subtree with 
        |EVar _ | EInt _ | EBool _ | EHole | ENil -> 0  
        |EUnOp _| EFun _ | EFix _  -> 1 
        |EBinOp _ |ELet _ |EPair _  -> 2 
        |EIf _  -> 3 )
        ; 
        in_scope = state.in_scope
      }
  in 
  recurse expr acts_init 
)  

(* Given an assignment number, load the unit test
  Input: 
    - assignment : index of assignment
  Output:
    - the unit test for the assignment
*)
let load_tests (directory : string) (assignment : int) : testType list =
  let filename = directory  ^ "/" ^ string_of_int assignment ^ "/test.ml" in
  let tests_cons = parse_file filename in
  let rec combine_tests (tests_cons : Expr.t) : (testType list) = 
    match tests_cons with
      | EBinOp (EPair (EInt a, EInt b), OpCon, ENil) -> [(a, b)]
      | EBinOp (EPair (EInt a, EInt b), OpCon, tl) -> (a, b) :: combine_tests tl
      | _ -> raise (IOError "Test file in incorrect format.")
  in 
  combine_tests tests_cons

(* Given an assignment number, load the code
  Input: 
    - assignment : index of assignment
  Output:
    - the code for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int) : Expr.t =
  let filename = directory  ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index ^ ".ml" in
  parse_file filename

(* Change tree representation to string to better interpret graph *)
let rec code_to_string (e : Expr.t) : string = 
  match e with
    | EVar x -> x ^ " "
    | EInt n -> string_of_int n ^ " "
    | EBool b -> string_of_bool b ^ " "
    | EUnOp (_, e) -> "(-" ^ code_to_string e ^ ") "
    | EBinOp (e1, op, e2) -> 
      let op_string = begin match op with
        | OpPlus -> "+"
        | OpMinus -> "-"
        | OpTimes -> "*"
        | OpDiv -> "/"
        | OpLt -> "<"
        | OpLe -> "<="
        | OpGt -> ">"
        | OpGe -> ">="
        | OpEq -> "="
        | OpNe -> "!="
        | OpCon -> "::"
        | OpAp -> " "
      end
      in
      "(" ^ code_to_string e1 ^ " " ^ op_string ^ " " ^ code_to_string e2  ^ ") "
    | EIf (cond, e1, e2) -> "(if " ^ code_to_string cond ^ " then " ^ code_to_string e1 ^ " else " ^ code_to_string e2 ^ ") "
    | ELet (x, EFix (_, e1), EHole) -> "let rec " ^ x ^ resolve_fun e1 ^ " "
    | ELet (x, EFix (_, e1), e2) -> "let rec " ^ x ^ resolve_fun e1 ^ " in " ^ code_to_string e2 ^ " "
    | ELet (x, EFun (arg, e1), EHole) -> "let " ^ x ^ resolve_fun (EFun (arg, e1)) ^ " "
    | ELet (x, EFun (arg, e1), e2) -> "let " ^ x ^ resolve_fun (EFun (arg, e1)) ^ " in " ^ code_to_string e2 ^ " "
    | ELet (x, e1, EHole) -> "let " ^ x ^ " = " ^ code_to_string e1 ^ " "
    | ELet (x, e1, e2) -> "let " ^ x ^ " = " ^ code_to_string e1 ^ " in " ^ code_to_string e2 ^ " "
    | EFix (_, _) -> raise (SyntaxError "Incorrect syntax with fix")
    | EFun (x,_, e) -> "(fun " ^ x ^ " -> " ^ code_to_string e ^ ") "
    | EPair (e1, e2) -> "(" ^ code_to_string e1 ^ ", " ^ code_to_string e2 ^ ") "
    | EHole -> "<HOLE> "
    | ENil -> "[] "
and resolve_fun (e : Expr.t) : string =
  match e with 
    | EFun (x,_, e) -> " " ^ x ^ resolve_fun e
    | _ -> " = " ^ code_to_string e  ^ " "


(*syn and ana *)
let rec synthesis (context: Assumptions.t) (e: Expr.t) : Typ.t option = 
  begin match e with
  | EVar x -> Assumptions.lookup context x 
  | EInt _  -> Int
  | EBool _ -> Bool 
  | EUnOp (OpNeg, arg) -> if analysis context arg Num then Some Num else None
  | EBinOp (argl, OpPlus | OpMinus | OpTimes | OpDiv, argr) -> 
      if analysis context argl Num  && analysis context argr Num then Some Num else None 
  | EBinOp (argl, OpGt | OpGe | OpLt | OpLe , argr) -> 
    if analysis context argl Num  && analysis context argr Num then Some Bool else None 
  | EBinOp (argl, OpEq | OpNe , argr) -> (* equal is a special case*)
    if (analysis context argl Num && analysis context argr Num) 
      || (analysis context argl Bool && analysis context argr Bool) then Some Bool else None
  | EBinop (arrow, OpAp, arg) -> 
    (match synthesis context arrow with 
      | Some Arrow (in_t, out_t) -> if analysis context arg in_t then Some out_t else None 
      | _ -> None )
  | EBinOp ( hd, OpCon, tl) -> 
    (match synthesis context tl with 
      | Some List (list_t) -> if analysis context hd list_t then Some (List list_t)  else None 
      | _ -> None )
  | EPair (l_pair, r_pair) -> (
    match synthesis context l_pair, synthesis context r_pair with 
    | Some l_t, Some r_t  -> Some (Pair (l_t,r_t))
    | _ -> None )
  | EIf (argl, argc,argr) -> (if analysis context argl Bool then (
    match synthesis context r_pair with 
    | Some out_t -> if analysis context argc out_t && analysis context argr out_t 
        then Some out_t else None 
    | _ -> None 
    )
    else None) 
  | ELet (varn, dec, body) -> ( match synthesis context dec with
    | Some var_t -> synthesis (Assumptions.extend context (varn,var_t)) body 
    | _ -> None)
  (* | ELet (varn, Some vart, dec, body) -> if analysis context dec vart 
    then synthesis (Assumptions.extend context (varn,var_t)) body 
    else None *)
  | EFun (varn, vart, body) ->(
    match synthesis (Assumptions.extend context (varn,vart)) body with 
    | Some outtype -> Some (Arrow (vart,outtype))
    | _ -> None )
  (* | EFun (varn, vart, Some outtype, body) -> 
    if analysis (Assumptions.extend context (varn,vart)) body outtype
      then Some Arrow (vart,outtype) else None  *)
  | EFix (varn, vart, body) -> 
    if analysis (Assumptions.extend context (varn,vart)) body vart 
      then Some vart else None 
  | EHole |ENil -> None 
  | _ -> None 
end

and analysis  (context:Assumptions.t) (e: Expr.t) (targ: Typ.t): bool =
  match e with
  | EFun (varn, vart, expr) -> Typ.equal (synthesis (Assumptions.extend context (varn,vart)) expr) targ
  | EFix (varn,vart , arg) -> (Typ.equal vart targ) && (analysis context arg, targ) 
  | EPair (lpair, rpair)  -> let l_t, r_t = targ in analysis context lpair l_t && analysis context rpair tart_r 
  | EIf (argl, argc, argr) -> analysis context argl Bool && analysis context argc targ && analysis context argr targ 
  | ELet(varn,vart, dec, body) -> analysis context dec vart && analysis (Assumptions.extend context (varn,vart)) body targ
  | _ -> Typ.equal (synthesis context e) targ 

