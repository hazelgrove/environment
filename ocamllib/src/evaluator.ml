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
  | Const _ | Hole -> e2
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
  | ListEq (e_l, e_r) -> ListEq (subx e_l, subx e_r)
  | Assert e -> Assert (subx e)
  | Match (e, (p1, e1), (p2, e2)) ->
      let rec find_x (p : Pattern.p_t) (x : Var.t) =
        match p with
        | Var y -> Var.equal x y
        | Cons (p1, p2) -> find_x p1 x || find_x p2 x
        | Const _ | Wild -> false
      in
      let e1 = if find_x p1 x then e1 else subx e1 in
      let e2 = if find_x p2 x then e2 else subx e2 in
      Match (subx e, (p1, e1), (p2, e2))

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
    match v with
    | VConst (Int n) -> n
    | _ -> raise (RuntimeError "Expected int")
  in
  (* Checks if v is a bool, and return the value of the bool *)
  let expecting_bool (v : Expr.value) : bool =
    match v with
    | VConst (Bool b) -> b
    | _ -> raise (RuntimeError "Expected bool")
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
    | Const c -> VConst c
    | Fun (x, ty, e_body) -> VFun (x, ty, e_body)
    | UnOp (op, e) -> (
        let v = eval e stack in
        match op with OpNeg -> VConst (Int (-1 * expecting_int v)))
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
            VConst (Int (f (expecting_int v1) (expecting_int v2)))
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
            VConst (Bool (f (expecting_int v1) (expecting_int v2)))
        | OpAnd | OpOr ->
            let f = match op with OpAnd -> ( && ) | _ -> ( || ) in
            VConst (Bool (f (expecting_bool v1) (expecting_bool v2)))
        | OpCons -> VCons (eval e1 stack, eval e2 stack))
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
    | Assert e ->
        let v = eval e stack in
        if expecting_bool v
        then VUnit
        else raise (RuntimeError "Assertion failed")
    | Map (func, e_list) -> (
        match eval e_list stack with
        | VConst Nil -> VConst Nil (* base case *)
        | VCons (head, tail) ->
            VCons
              ( eval (BinOp (func, OpAp, Expr.from_val head)) (stack - 1),
                eval (Map (func, Expr.from_val tail)) (stack - 1) )
        | _ -> raise (RuntimeError "Expected list or nil"))
    | Filter (func, e_list) -> (
        match eval e_list stack with
        | VConst Nil -> VConst Nil (* base case *)
        | VCons (head, tail) -> (
            let filtered_tail =
              eval (Filter (func, Expr.from_val tail)) (stack - 1)
            in
            match eval (BinOp (func, OpAp, Expr.from_val head)) (stack - 1) with
            | VConst (Bool true) ->
                let evaled_head = eval (Expr.from_val head) (stack - 1) in
                VCons (evaled_head, filtered_tail)
            | VConst (Bool false) -> filtered_tail
            | _ -> raise (RuntimeError "Expected Bool"))
        | _ -> raise (RuntimeError "Expected list or nil"))
    | ListEq (e1, e2) -> (
        match (eval e1 stack, eval e2 stack) with
        | VConst Nil, VConst Nil -> VConst (Bool true)
        | VConst Nil, _ | _, VConst Nil -> VConst (Bool false)
        | VCons (head1, tail1), VCons (head2, tail2) -> (
            let head_eq =
              eval
                (BinOp (Expr.from_val head1, OpEq, Expr.from_val head2))
                (stack - 1)
            in
            let tail_eq =
              eval
                (ListEq (Expr.from_val tail1, Expr.from_val tail2))
                (stack - 1)
            in
            match (head_eq, tail_eq) with
            | VConst (Bool true), VConst (Bool true) -> VConst (Bool true)
            | _ -> VConst (Bool false))
        | _ -> raise (RuntimeError "Expected list or nil"))
    | Match (e, (p1, e1), (p2, e2)) -> (
        let v = eval e stack in
        let rec find_match (pattern : Pattern.p_t) (value : Expr.value) :
            (Var.t * Expr.value) list option =
          match (pattern, value) with
          | Const c1, VConst c2 -> if Const.equal c1 c2 then Some [] else None
          | Var x, _ -> Some [ (x, value) ]
          | Cons (p1, p2), VCons (v1, v2) -> (
              match (find_match p1 v1, find_match p2 v2) with
              | Some a, Some b -> Some (a @ b)
              | _ -> None)
          | Wild, _ -> Some []
          | _ -> None
        in
        match find_match p1 v with
        | Some bindings ->
            let e1 =
              List.fold_left
                (fun e (x, v) -> subst (Expr.from_val v) x e)
                e1 bindings
            in
            eval e1 stack
        | None -> (
            match find_match p2 v with
            | Some bindings ->
                let e2 =
                  List.fold_left
                    (fun e (x, v) -> subst (Expr.from_val v) x e)
                    e2 bindings
                in
                eval e2 stack
            | None -> raise (RuntimeError "Pattern matching failed")))
    | Var _ -> raise (SyntaxError "Variable not bound")
    | Hole -> raise (SyntaxError "Hole in expression")

let%test_module "Test eval" =
  (module struct
    let parse = ParserUtils.parse
    let eval_string s = eval (parse s) 100

    (* Ints *)
    let%test _ = eval_string "32" = VConst (Const.Int 32)
    let%test _ = eval_string "-5" = VConst (Const.Int (-5))
    let%test _ = eval_string "0" = VConst (Const.Int 0)
    (* Bools *)
    let%test _ = eval_string "true" = VConst (Bool true)
    let%test _ = eval_string "false" = VConst (Bool false)

    (* Functions *)
    let%test _ =
      eval_string "fun x1 -> 2 * x1 + 1" = VFun (1, Hole, parse "2 * x1 + 1")

    let%test _ = eval_string "fun x2 -> x2 2" = VFun (2, Hole, parse "x2 2")
    let%test _ = eval_string "(fun x1 -> 3 * x1 - 1) 4" = VConst (Const.Int 11)
    (* Arithmetic Operations *)
    let%test _ = eval_string "- (4 * 3) / 2 + -1" = VConst (Const.Int (-7))

    (* If Expressions *)
    let%test _ =
      eval_string "if (6 > 4) then if (2 != 3) then 5 else 8 else 10"
      = VConst (Const.Int 5)

    (* Let Expressions *)
    let%test _ =
      eval_string "let x1 = 1 in let x1 = x1 + 3 in 2 * x1"
      = VConst (Const.Int 8)

    let%test _ =
      eval_string "let f x1 = 2 - x1 * 6 in f 3" = VConst (Const.Int (-16))

    let%test _ =
      eval_string "let f x1 x2 = x1 + x2 in f 2 3" = VConst (Const.Int 5)

    (* let%test _ =
       eval_string
         "let rec fact x1 = if x1 < 1 then 1 else x1 * fact (x1 - 1) in fact 4"
       = VConst (Const.Int 24)
    *)
    (* Pairs *)
    let%test _ =
      eval_string "(8 <= 20, fun x1 -> 2 * x1 + 1)"
      = VPair (VConst (Bool true), VFun (1, Hole, parse "2 * x1 + 1"))

    (* lists  (map and filter) *)

    let%test _ =
      eval_string " 1 :: 2 ::3 ::[]"
      = VCons
          ( VConst (Int 1),
            VCons (VConst (Int 2), VCons (VConst (Int 3), VConst Nil)) )

    let%test _ =
      eval_string "map (fun x1 -> 2* x1 ) (1 :: 2 ::3 :: 4 ::[])"
      = VCons
          ( VConst (Int 2),
            VCons
              ( VConst (Int 4),
                VCons (VConst (Int 6), VCons (VConst (Int 8), VConst Nil)) ) )

    let%test _ =
      eval_string
        "filter (fun x1 -> x1 - 1 = 2 || x1 - 1 = 0 ) (3 :: 2 ::1 :: 4 ::[])"
      = VCons (VConst (Int 3), VCons (VConst (Int 1), VConst Nil))
  end)

(*
   Given a unit test set and AST, check if AST passes tests
   Input:
     - test_set : a list of tests of testType with inputs and their corresponding output
     - code : the code to be evaluated upon
   Output:
     true, if code passes all tests
     false, otherwise
*)
(* let rec run_unit_tests (test_set : (int * int) list) (code : Expr.t) : bool =
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
   | hd :: tl -> if run_test hd code then run_unit_tests tl code else false *)

let rec run_unit_tests_private (test_set : (int * int) list) (code : Expr.t) :
    bool =
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
                       BinOp (Var id, Expr.OpAp, Const (Int test_input)) ))
                  100
              with _ -> VError
            in
            match output with
            | VConst (Int n) -> n = test_output
            | VError -> false
            | _ -> false)
        | _ -> false)
    | _ -> false
  in
  match test_set with
  | [] -> true
  | hd :: tl ->
      if run_test hd code then run_unit_tests_private tl code else false

let run_unit_tests (code : Expr.t) : bool =
  let output = try eval (Expr.strip code) 100 with _ -> VError in
  match output with VError -> false | _ -> true

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
