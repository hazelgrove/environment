(* Converts actions to ints/lists *)
open Action

let tag_to_action (action : int) : t =
  match action with
  | 0 -> Move Parent
  | 1 -> Move (Child 0)
  | 2 -> Move (Child 1)
  | 3 -> Move (Child 2)
  | 10 -> Construct (Var "x")
  | 11 -> Construct (Var "y")
  | 12 -> Construct (Var "z")
  | 13 -> Construct Hole
  | 14 -> Construct Nil
  | 15 -> Construct (Int (-2))
  | 16 -> Construct (Int (-1))
  | 17 -> Construct (Int 0)
  | 18 -> Construct (Int 1)
  | 19 -> Construct (Int 2)
  | 20 -> Construct (Bool true)
  | 21 -> Construct (Bool false)
  | 22 -> Construct (UnOp OpNeg)
  | 23 -> Construct (BinOp_L OpPlus)
  | 24 -> Construct (BinOp_L OpMinus)
  | 25 -> Construct (BinOp_L OpTimes)
  | 26 -> Construct (BinOp_L OpDiv)
  | 27 -> Construct (BinOp_L OpLt)
  | 28 -> Construct (BinOp_L OpLe)
  | 29 -> Construct (BinOp_L OpGt)
  | 30 -> Construct (BinOp_L OpGe)
  | 31 -> Construct (BinOp_L OpEq)
  | 32 -> Construct (BinOp_L OpNe)
  | 33 -> Construct (BinOp_L OpAp)
  | 34 -> Construct (BinOp_L OpCons)
  | 35 -> Construct (BinOp_R OpPlus)
  | 36 -> Construct (BinOp_R OpMinus)
  | 37 -> Construct (BinOp_R OpTimes)
  | 38 -> Construct (BinOp_R OpDiv)
  | 39 -> Construct (BinOp_R OpLt)
  | 40 -> Construct (BinOp_R OpLe)
  | 41 -> Construct (BinOp_R OpGt)
  | 42 -> Construct (BinOp_R OpGe)
  | 43 -> Construct (BinOp_R OpEq)
  | 44 -> Construct (BinOp_R OpNe)
  | 45 -> Construct (BinOp_R OpAp)
  | 46 -> Construct (BinOp_R OpCons)
  | 47 -> Construct (Let_L Var.undef_var)
  | 50 -> Construct (Let_R Var.undef_var)
  | 53 -> Construct If_L
  | 54 -> Construct If_C
  | 55 -> Construct If_R
  | 56 -> Construct (Fun Var.undef_var)
  | 59 -> Construct (Fix Var.undef_var)
  | 62 -> Construct Pair_L
  | 63 -> Construct Pair_R
  | 64 -> Construct TypInt
  | 65 -> Construct TypBool
  | 66 -> Construct TypArrow_L
  | 67 -> Construct TypArrow_R
  | 68 -> Construct TypProd_L
  | 69 -> Construct TypProd_R
  | 70 -> Construct TypList
  | 71 -> Construct TypHole
  | _ -> raise (Failure "Action index not supported.")

(* TODO: Change number after finalize *)
let action_to_tag (action : t) : int =
  match action with
  | Move Parent -> 0
  | Move (Child 0) -> 1
  | Move (Child 1) -> 2
  | Move (Child 2) -> 3
  | Construct (Var "x") -> 10
  | Construct (Var "y") -> 11
  | Construct (Var "z") -> 12
  | Construct Hole -> 13
  | Construct Nil -> 14
  | Construct (Int -2) -> 15
  | Construct (Int -1) -> 16
  | Construct (Int 0) -> 17
  | Construct (Int 1) -> 18
  | Construct (Int 2) -> 19
  | Construct (Bool true) -> 20
  | Construct (Bool false) -> 21
  | Construct (UnOp OpNeg) -> 22
  | Construct (BinOp_L OpPlus) -> 23
  | Construct (BinOp_L OpMinus) -> 24
  | Construct (BinOp_L OpTimes) -> 25
  | Construct (BinOp_L OpDiv) -> 26
  | Construct (BinOp_L OpLt) -> 27
  | Construct (BinOp_L OpLe) -> 28
  | Construct (BinOp_L OpGt) -> 29
  | Construct (BinOp_L OpGe) -> 30
  | Construct (BinOp_L OpEq) -> 31
  | Construct (BinOp_L OpNe) -> 32
  | Construct (BinOp_L OpAp) -> 33
  | Construct (BinOp_L OpCons) -> 34
  | Construct (BinOp_R OpPlus) -> 35
  | Construct (BinOp_R OpMinus) -> 36
  | Construct (BinOp_R OpTimes) -> 37
  | Construct (BinOp_R OpDiv) -> 38
  | Construct (BinOp_R OpLt) -> 39
  | Construct (BinOp_R OpLe) -> 40
  | Construct (BinOp_R OpGt) -> 41
  | Construct (BinOp_R OpGe) -> 42
  | Construct (BinOp_R OpEq) -> 43
  | Construct (BinOp_R OpNe) -> 44
  | Construct (BinOp_R OpAp) -> 45
  | Construct (BinOp_R OpCons) -> 46
  | Construct (Let_L "") -> 47
  | Construct (Let_R "") -> 50
  | Construct If_L -> 53
  | Construct If_C -> 54
  | Construct If_R -> 55
  | Construct (Fun "") -> 56
  | Construct (Fix "") -> 59
  | Construct Pair_L -> 62
  | Construct Pair_R -> 63
  | Construct TypInt -> 64
  | Construct TypBool -> 65
  | Construct TypArrow_L -> 66
  | Construct TypArrow_R -> 67
  | Construct TypProd_L -> 68
  | Construct TypProd_R -> 69
  | Construct TypList -> 70
  | Construct TypHole -> 71
  | _ -> raise (Failure "Action not supported.")

let to_list (action_list : t list) : bool list =
  let action_list = List.map action_to_tag action_list in
  let action_list = List.sort compare action_list in
  let bool_list = Array.make 71 false in
  (* TODO: Change max num of actions *)
  let rec to_bool (action_list : int list) (bool_list : bool Array.t) =
    match action_list with
    | [] -> bool_list
    | hd :: tl ->
        bool_list.(hd) <- true;
        to_bool tl bool_list
  in
  Array.to_list (to_bool action_list bool_list)
