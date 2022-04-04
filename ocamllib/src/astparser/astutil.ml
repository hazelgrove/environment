open Ast

exception SyntaxError of string
exception RuntimeError of string
exception NotImplemented

(*
    Parse a string into an ast
    Input :
      - s : a string resembling the code
    Output : 
      - an ast corresponding to s
*)
let parse (s : string) : Expr.t =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let%test_module "Test parse" =
  (module struct
    (* BinOp *)
    let%test _ =
      parse "32 + 54 * 2 / 10"
      = EBinOp
          ( EInt 32,
            OpPlus,
            EBinOp (EBinOp (EInt 54, OpTimes, EInt 2), OpDiv, EInt 10) )

    let%test _ = parse "10 <= 23" = EBinOp (EInt 10, OpLe, EInt 23)

    let%test _ =
      parse "f 2 3" = EBinOp (EBinOp (EVar "f", OpAp, EInt 2), OpAp, EInt 3)

    (* Functions *)
    let%test _ =
      parse "fun (x : int) -> x + 1"
      = EFun ("x", TInt, EBinOp (EVar "x", OpPlus, EInt 1))

    let%test _ =
      parse "let f (x : int) : bool = x != 2 in f 1"
      = ELet
          ( "f",
            TArrow (TInt, TBool),
            EFun ("x", TInt, EBinOp (EVar "x", OpNe, EInt 2)),
            EBinOp (EVar "f", OpAp, EInt 1) )

    let%test _ =
      parse "let f x (y : int) = x + y"
      = ELet
          ( "f",
            THole,
            EFun
              ("x", THole, EFun ("y", TInt, EBinOp (EVar "x", OpPlus, EVar "y"))),
            EHole )

    let%test _ =
      parse
        "let rec fact (n : int) : int = if n = 0 then 1 else n * fact (n - 1) \
         in fact 4"
      = ELet
          ( "fact",
            TArrow (TInt, TInt),
            EFix
              ( "fact",
                TArrow (TInt, TInt),
                EFun
                  ( "n",
                    TInt,
                    EIf
                      ( EBinOp (EVar "n", OpEq, EInt 0),
                        EInt 1,
                        EBinOp
                          ( EVar "n",
                            OpTimes,
                            EBinOp
                              ( EVar "fact",
                                OpAp,
                                EBinOp (EVar "n", OpMinus, EInt 1) ) ) ) ) ),
            EBinOp (EVar "fact", OpAp, EInt 4) )

    (* Let and if expressions *)
    let%test _ =
      parse "let x : bool = true in if x then 2 else 3"
      = ELet ("x", TBool, EBool true, EIf (EVar "x", EInt 2, EInt 3))

    (* Pairs *)
    let%test _ =
      parse "(2 + 3, true)" = EPair (EBinOp (EInt 2, OpPlus, EInt 3), EBool true)
  end)

(*
  Parse a file (assuming it is a well-typed .ml file) into an ast
  Input :
    - filename : directory of the .ml file
  Output :
    - an ast corresponding to the .ml file at the directory filename
*)
let parse_file filename =
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let s = read_whole_file filename in
  parse s

(*
  Substitute the variable x in e2 with e1 (i.e. [e1/x]e2)
  Inputs :
    - e1 : expression to be substituted for x
    - x : variable to substitue
    - e2 : expression where x needs to be substituted
  Output :
    - e2 with the variable x substituted with e1
  *)
let rec subst (e1 : Expr.t) (x : Var.t) (e2 : Expr.t) : Expr.t =
  (* Shorthand for the function substituting the variable x with e1 *)
  let subx = subst e1 x in
  (* Substitute if condition is not met *)
  let subx_unless cond = if cond then Fun.id else subx in
  match e2 with
  | EBool _ | EInt _ | ENil | EHole -> e2
  | EVar y -> if Var.equal x y then e1 else e2
  | EUnOp (op, e) -> EUnOp (op, subx e)
  | EBinOp (e_l, op, e_r) -> EBinOp (subx e_l, op, subx e_r)
  | EIf (e_cond, e_then, e_else) -> EIf (subx e_cond, subx e_then, subx e_else)
  | EFun (y, ty, e_body) -> EFun (y, ty, subx_unless (Var.equal x y) e_body)
  | ELet (y, ty, e_def, e_body) ->
      ELet (y, ty, subx e_def, subx_unless (Var.equal x y) e_body)
  | EFix (y, ty, e_body) -> EFix (y, ty, subx_unless (Var.equal x y) e_body)
  | EPair (e_l, e_r) -> EPair (subx e_l, subx e_r)

(*
  Evalutate the expression e
  Inputs :
    - e : expression to be evaluated
    - stack : number of stack frames to be allocated towards the evaluation
  Outputs :
    - the value of the expression e
  Raises :
    - SyntaxError :
      - "Variable not bound" : iff there is a free variable
      - "Hole in expression" : iff there is an empty hole in the expression
    - RuntimeError :
      - "Stack overflow" : iff the number of stack frames used exceeds stack
      - "Expected [type]" : iff there is a type error
    - NotImplemented :
      - iff OpCons is used
  *)
let rec eval (e : Expr.t) (stack : int) : Expr.value =
  (* Checks if v is an int, and return the value of the int *)
  let expecting_int (v : Expr.value) : int =
    match v with VInt n -> n | _ -> raise (RuntimeError "Expected int")
  in
  (* Checks if v is a bool, and return the value of the bool *)
  let expecting_bool (v : Expr.value) : bool =
    match v with VBool b -> b | _ -> raise (RuntimeError "Expected bool")
  in
  (* Checks if v is a function, and returns its argument name and body *)
  let expecting_fun (v : Expr.value) : Var.t * Expr.t =
    match v with
    | VFun (x, _, body) -> (x, body)
    | _ -> raise (RuntimeError "Expected function")
  in
  if stack = 0
  then raise (RuntimeError "Stack overflow")
  else
    match e with
    | EInt n -> VInt n
    | EBool b -> VBool b
    | EFun (x, ty, e_body) -> VFun (x, ty, e_body)
    | ENil -> VNil
    | EUnOp (op, e) -> (
        let v = eval e stack in
        match op with OpNeg -> VInt (-1 * expecting_int v))
    | EBinOp (e1, op, e2) -> (
        let v1 = eval e1 stack in
        let v2 = eval e2 stack in
        match op with
        | OpAp ->
            let x, body = expecting_fun v1 in
            eval (subst (Expr.from_val v2) x body) (stack - 1)
        | OpPlus | OpMinus | OpTimes | OpDiv ->
            let f =
              match op with
              | OpPlus -> ( + )
              | OpMinus -> ( - )
              | OpTimes -> ( * )
              | _ -> ( / )
            in
            VInt (f (expecting_int v1) (expecting_int v2))
        | OpLt | OpLe | OpGt | OpGe | OpEq | OpNe ->
            let f =
              match op with
              | OpLt -> ( < )
              | OpLe -> ( <= )
              | OpGt -> ( > )
              | OpGe -> ( >= )
              | OpNe -> ( != )
              | _ -> ( = )
            in
            VBool (f (expecting_int v1) (expecting_int v2))
        | OpCon -> raise NotImplemented)
    | EIf (e_cond, e_then, e_else) ->
        let b = expecting_bool (eval e_cond stack) in
        if b then eval e_then stack else eval e_else stack
    | ELet (x, _, e_def, e_body) ->
        let v_def = eval e_def stack in
        eval (subst (Expr.from_val v_def) x e_body) stack
    | EPair (e_l, e_r) -> VPair (eval e_l stack, eval e_r stack)
    | EFix (x, ty, e_body) ->
        let unrolled = subst (EFix (x, ty, e_body)) x e_body in
        eval unrolled stack
    | EVar _ -> raise (SyntaxError "Variable not bound")
    | EHole -> raise (SyntaxError "Hole in expression")

let%test_module "Test eval" =
  (module struct
    let eval_string s = eval (parse s) 100

    (* Ints *)
    let%test _ = eval_string "32" = VInt 32
    let%test _ = eval_string "-5" = VInt (-5)
    let%test _ = eval_string "0" = VInt 0
    (* Bools *)
    let%test _ = eval_string "true" = VBool true
    let%test _ = eval_string "false" = VBool false

    (* Functions *)
    let%test _ =
      eval_string "fun x -> 2 * x + 1" = VFun ("x", THole, parse "2 * x + 1")

    let%test _ = eval_string "fun a -> a 2" = VFun ("a", THole, parse "a 2")
    let%test _ = eval_string "(fun x -> 3 * x - 1) 4" = VInt 11
    (* Arithmetic Operations *)
    let%test _ = eval_string "- (4 * 3) / 2 + -1" = VInt (-7)

    (* If Expressions *)
    let%test _ =
      eval_string "if (6 > 4) then if (2 != 3) then 5 else 8 else 10" = VInt 5

    (* Let Expressions *)
    let%test _ = eval_string "let x = 1 in let x = x + 3 in 2 * x" = VInt 8
    let%test _ = eval_string "let f x = 2 - x * 6 in f 3" = VInt (-16)
    let%test _ = eval_string "let f x y = x + y in f 2 3" = VInt 5

    let%test _ =
      eval_string
        "let rec fact n = if n < 1 then 1 else n * fact (n - 1) in fact 4"
      = VInt 24

    (* Pairs *)
    let%test _ =
      eval_string "(8 <= 20, fun x -> 2 * x + 1)"
      = VPair (VBool true, VFun ("x", THole, parse "2 * x + 1"))
  end)

let serialize (zast : Expr.z_t) : string =
  Core.Sexp.to_string (Expr.sexp_of_z_t zast)

let deserialize (zast : string) : Expr.z_t =
  Expr.z_t_of_sexp (Core.Sexp.of_string zast)
