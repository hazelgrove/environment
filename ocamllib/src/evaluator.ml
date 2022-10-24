(* Functions to evaluate expressions or check test cases *)
exception RuntimeError of string
exception SyntaxError of string
exception NotImplemented

(*
  Substitute the variable x in e2 with e1 (i.e. [e1/x]e2)
  Inputs :
    - e1 : expression to be substituted for x
    - x : variable to substitue
    - e2 : expression where x needs to be substituted
  Output :
    - e2 with the variable x substituted with e1
  *)
let rec subst (e1 : Expr.p_t) (x : Var.t) (e2 : Expr.p_t) : Expr.p_t =
  (* Shorthand for the function substituting the variable x with e1 *)
  let subx = subst e1 x in
  (* Substitute if condition is not met *)
  let subx_unless cond = if cond then Fun.id else subx in
  match e2 with
  | BoolLit _ | IntLit _ | Nil | Hole -> e2
  | Var y -> if Var.equal x y then e1 else e2
  | UnOp (op, e) -> UnOp (op, subx e)
  | BinOp (e_l, op, e_r) -> BinOp (subx e_l, op, subx e_r)
  | If (e_cond, e_then, e_else) -> If (subx e_cond, subx e_then, subx e_else)
  | Fun (y, ty, e_body) -> Fun (y, ty, subx_unless (Var.equal x y) e_body)
  | Let (y, e_def, e_body) ->
      Let (y, subx e_def, subx_unless (Var.equal x y) e_body)
  | Fix (y, ty, e_body) -> Fix (y, ty, subx_unless (Var.equal x y) e_body)
  | Pair (e_l, e_r) -> Pair (subx e_l, subx e_r)
  | Map (func, e_list) -> Map (subx func, subx e_list)
  | Filter (func, e_list) -> Filter (subx func, subx e_list)


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
let rec eval (e : Expr.p_t) (stack : int) : Expr.value =
  (* Checks if v is an int, and return the value of the int *)
  let expecting_int (v : Expr.value) : int =
    match v with VInt n -> n | _ -> raise (RuntimeError "Expected int")
  in
  (* Checks if v is a bool, and return the value of the bool *)
  let expecting_bool (v : Expr.value) : bool =
    match v with VBool b -> b | _ -> raise (RuntimeError "Expected bool")
  in
  (* Checks if v is a function, and returns its argument name and body *)
  let expecting_fun (v : Expr.value) : Var.t * Expr.p_t =
    match v with
    | VFun (x, _, body) -> (x, body)
    | _ -> raise (RuntimeError "Expected function")
  in
  if stack = 0
  then raise (RuntimeError "Stack overflow")
  else
    match e with
    | IntLit n -> VInt n
    | BoolLit b -> VBool b
    | Fun (x, ty, e_body) -> VFun (x, ty, e_body)
    | Nil -> VNil
    | UnOp (op, e) -> (
        let v = eval e stack in
        match op with OpNeg -> VInt (-1 * expecting_int v))
    | BinOp (e1, op, e2) -> (
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
        | OpCons -> raise NotImplemented)
    | If (e_cond, e_then, e_else) ->
        let b = expecting_bool (eval e_cond stack) in
        if b then eval e_then stack else eval e_else stack
    | Let (x, e_def, e_body) ->
        let v_def = eval e_def stack in
        eval (subst (Expr.from_val v_def) x e_body) stack
    | Pair (e_l, e_r) -> VPair (eval e_l stack, eval e_r stack)
    | Fix (x, ty, e_body) ->
        let unrolled = subst (Fix (x, ty, e_body)) x e_body in
        eval unrolled stack

    | Map (func, e_list) 
    | Filter (func, e_list)   -> raise NotImplemented
    | Var _ -> raise (SyntaxError "Variable not bound")
    | Hole -> raise (SyntaxError "Hole in expression")

(* let%test_module "Test eval" =
   (module struct
     let parse = ParserUtils.parse
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
*)

(*
   Given a unit test set and AST, check if AST passes tests
   Input:
     - test_set : a list of tests of testType with inputs and their corresponding output
     - code : the code to be evaluated upon
   Output:
     true, if code passes all tests
     false, otherwise
*)
let rec run_unit_tests (test_set : (int * int) list) (code : Expr.t) : bool =
  let run_test (test : int * int) (code : Expr.t) : bool =
    (* Assume code is a function in an ELet (_, EFun/EFix (_ , _), EHole) *)
    match code.node with
    | ELet (id, f, _) -> (
        match f.node with
        | EFun _ | EFix _ -> (
            let test_input, test_output = test in
            let output =
              try
                eval
                  (Expr.Let
                     ( id,
                       Expr.strip f,
                       BinOp (Var id, Expr.OpAp, IntLit test_input) ))
                  100
              with _ -> VError
            in
            match output with
            | VInt n -> n = test_output
            | VError -> false
            | _ -> false)
        | _ -> false)
    | _ -> false
  in
  match test_set with
  | [] -> true
  | hd :: tl -> if run_test hd code then run_unit_tests tl code else false

(* let%test_module "Test run_unit_tests" =
   (module struct
     let parse = ParserUtils.parse

     (* All correct *)
     let%test _ =
       run_unit_tests [ (1, 2); (-2, -1); (0, 1) ] (parse "let f n = n + 1")
       = true

     (* Partially correct *)
     let%test _ =
       run_unit_tests [ (1, 2); (-2, 0); (0, 1) ] (parse "let f n = n + 1")
       = false

     (* Incorrect *)
     let%test _ =
       run_unit_tests [ (1, 3); (-2, 0); (0, 2) ] (parse "let f n = n + 1")
       = false

     (* Error in code *)
     let%test _ =
       run_unit_tests [ (1, 2); (-2, -1); (0, 1) ] (parse "let f n = n + true")
       = false

     (* Error in format *)
     let%test _ =
       run_unit_tests
         [ (1, 2); (-2, -1); (0, 1) ]
         (parse "let f n = n + true in f 1")
       = false
   end) *)
