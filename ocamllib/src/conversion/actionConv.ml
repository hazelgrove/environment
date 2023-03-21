(* Converts actions to ints/lists *)
open Action

let action_list =
  [
    Move Parent;
    Move (Child 0);
    Move (Child 1);
    Move (Child 2);
    Move (Child 3);
    Move (Child 4);
    Construct Hole;
    Construct (Const Nil);
    Construct (Const (Bool true));
    Construct (Const (Bool false));
    Construct (UnOp OpNeg);
    Construct (BinOp_L OpPlus);
    Construct (BinOp_L OpMinus);
    Construct (BinOp_L OpTimes);
    Construct (BinOp_L OpDiv);
    Construct (BinOp_L OpLt);
    Construct (BinOp_L OpLe);
    Construct (BinOp_L OpGt);
    Construct (BinOp_L OpGe);
    Construct (BinOp_L OpEq);
    Construct (BinOp_L OpNe);
    Construct (BinOp_L OpAp);
    Construct (BinOp_L OpCons);
    Construct (BinOp_L OpAnd);
    Construct (BinOp_L OpOr);
    Construct (BinOp_R OpPlus);
    Construct (BinOp_R OpMinus);
    Construct (BinOp_R OpTimes);
    Construct (BinOp_R OpDiv);
    Construct (BinOp_R OpLt);
    Construct (BinOp_R OpLe);
    Construct (BinOp_R OpGt);
    Construct (BinOp_R OpGe);
    Construct (BinOp_R OpEq);
    Construct (BinOp_R OpNe);
    Construct (BinOp_R OpAp);
    Construct (BinOp_R OpCons);
    Construct (BinOp_R OpAnd);
    Construct (BinOp_R OpOr);
    Construct Let_L;
    Construct Let_R;
    Construct If_L;
    Construct If_C;
    Construct If_R;
    Construct Fun;
    (* Construct Fix; *)
    Construct Pair_L;
    Construct Pair_R;
    Construct Filter_L;
    Construct Filter_R;
    Construct Map_L;
    Construct Map_R;
    Construct Fold_L;
    Construct Fold_C;
    Construct Fold_R;
    Construct Match_L;
    Construct Match_E1;
    Construct Match_E2;
    (* Construct TypInt;
       Construct TypBool;
       Construct TypArrow_L;
       Construct TypArrow_R;
       Construct TypProd_L;
       Construct TypProd_R;
       Construct TypList;
       Construct TypHole;
       Construct TypUnit; *)
    Construct (PatConst (Bool true));
    Construct (PatConst (Bool false));
    Construct (PatConst Nil);
    Construct PatCons_L;
    Construct PatCons_R;
    Construct PatVar;
    Construct PatWild;
    Unwrap 0;
    Unwrap 1;
    Unwrap 2;
  ]
  @ List.init Const.num_ints (fun i -> Construct (Const (Int (-1 * i))))
  @ List.init Const.num_ints (fun i -> Construct (Const (Int i)))
  @ List.init Const.num_ints (fun i -> Construct (PatConst (Int (-1 * i))))
  @ List.init Const.num_ints (fun i -> Construct (PatConst (Int i)))
  @ List.init Var.max_num_vars (fun i -> Construct (Var i))
  @ List.init Var.max_num_vars (fun i -> Construct (Arg i))

let num_actions = List.length action_list

let tag_to_action (action : int) : t =
  try List.nth action_list action
  with Failure _ | Invalid_argument _ ->
    raise (Failure "Invalid action index")

let action_to_tag (action : t) : int =
  let rec find x lst c =
    match lst with
    | [] -> raise (Failure "Invalid action")
    | hd :: tl -> if hd = x then c else find x tl (c + 1)
  in
  find action action_list 0

(*
   Converts a list of possible actions to a list of bools, where each possible action is marked as true while others are marked as false
*)
let to_list (action_list : t list) : bool list =
  let action_list = List.map action_to_tag action_list in
  let action_list = List.sort compare action_list in
  let bool_list = Array.make (num_actions + (Var.max_num_vars * 2)) false in
  let rec to_bool (action_list : int list) (bool_list : bool Array.t) =
    match action_list with
    | [] -> bool_list
    | hd :: tl ->
        bool_list.(hd) <- true;
        to_bool tl bool_list
  in
  Array.to_list (to_bool action_list bool_list)

let to_string (action : t) : string =
  match action with
  | Move Parent -> "Move Parent"
  | Move (Child x) -> "Move Child " ^ string_of_int x
  | Construct Hole -> "Construct Hole"
  | Construct (Const Nil) -> "Construct Nil"
  | Construct (Const (Int x)) -> "Construct Int " ^ string_of_int x
  | Construct (Const (Bool x)) -> "Construct Bool " ^ string_of_bool x
  | Construct (UnOp OpNeg) -> "Construct UnOp OpNeg"
  | Construct (BinOp_L op) ->
      let binop =
        match op with
        | OpPlus -> "OpPlus"
        | OpMinus -> "OpMinus"
        | OpTimes -> "OpTimes"
        | OpDiv -> "OpDiv"
        | OpLt -> "OpLt"
        | OpLe -> "OpLe"
        | OpGt -> "OpGt"
        | OpGe -> "OpGe"
        | OpEq -> "OpEq"
        | OpNe -> "OpNe"
        | OpAp -> "OpAp"
        | OpCons -> "OpCons"
        | OpAnd -> "OpAnd"
        | OpOr -> "OpOr"
      in
      "Construct BinOp_L " ^ binop
  | Construct (BinOp_R op) ->
      let binop =
        match op with
        | OpPlus -> "OpPlus"
        | OpMinus -> "OpMinus"
        | OpTimes -> "OpTimes"
        | OpDiv -> "OpDiv"
        | OpLt -> "OpLt"
        | OpLe -> "OpLe"
        | OpGt -> "OpGt"
        | OpGe -> "OpGe"
        | OpEq -> "OpEq"
        | OpNe -> "OpNe"
        | OpAp -> "OpAp"
        | OpCons -> "OpCons"
        | OpAnd -> "OpAnd"
        | OpOr -> "OpOr"
      in
      "Construct BinOp_R " ^ binop
  | Construct Let_L -> "Construct Let_L"
  | Construct Let_R -> "Construct Let_R"
  | Construct If_L -> "Construct If_L"
  | Construct If_C -> "Construct If_C"
  | Construct If_R -> "Construct If_R"
  | Construct Fun -> "Construct Fun"
  | Construct Fix -> "Construct Fix"
  | Construct Pair_L -> "Construct Pair_L"
  | Construct Pair_R -> "Construct Pair_R"
  | Construct Map_L -> "Constrcut Map_L"
  | Construct Map_R -> "Constrcut Map_R"
  | Construct Filter_L -> "Construct Filter_L"
  | Construct Filter_R -> "Construct Filter_R"
  | Construct Fold_L -> "Construct Fold_L"
  | Construct Fold_C -> "Construct Fold_C"
  | Construct Fold_R -> "Construct Fold_R"
  | Construct TypInt -> "Construct TypInt"
  | Construct TypBool -> "Construct TypBool"
  | Construct TypArrow_L -> "Construct TypArrow_L"
  | Construct TypArrow_R -> "Construct TypArrow_R"
  | Construct TypProd_L -> "Construct TypProd_L"
  | Construct TypProd_R -> "Construct TypProd_R"
  | Construct TypList -> "Construct TypList"
  | Construct TypHole -> "Construct TypHole"
  | Construct TypUnit -> "Construct TypUnit"
  | Construct (Var x) -> "Construct Var " ^ string_of_int x
  | Construct (Arg x) -> "Construct Arg " ^ string_of_int x
  | Construct Match_L -> "Construct Match_L"
  | Construct Match_E1 -> "Construct Match_E1"
  | Construct Match_E2 -> "Construct Match_E2"
  | Construct (PatConst c) -> "Construct PatConst " ^ ConstConv.to_string c
  | Construct PatCons_L -> "Construct PatCons_L"
  | Construct PatCons_R -> "Construct PatCons_R"
  | Construct PatVar -> "Construct PatVar"
  | Construct PatWild -> "Construct PatWild"
  | Unwrap x -> "Unwrap " ^ string_of_int x
