(* Converts actions to ints/lists *)
open Action

let action_list =
  [
    Move Parent;
    Move (Child 0);
    Move (Child 1);
    Move (Child 2);
    Move Next;
    Move Prev;
    Construct Hole;
    Construct Nil;
    Construct (Int (-2));
    Construct (Int (-1));
    Construct (Int 0);
    Construct (Int 1);
    Construct (Int 2);
    Construct (Bool true);
    Construct (Bool false);
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
    Construct Let_L;
    Construct Let_R;
    Construct If_L;
    Construct If_C;
    Construct If_R;
    Construct Fun;
    Construct Fix;
    Construct Pair_L;
    Construct Pair_R;
    Construct Match_L;
    Construct Match_C;
    Construct Match_R;
    Construct TypInt;
    Construct TypBool;
    Construct TypArrow_L;
    Construct TypArrow_R;
    Construct TypProd_L;
    Construct TypProd_R;
    Construct TypList;
    Construct TypHole;
    Unwrap 0;
    Unwrap 1;
    Unwrap 2;
  ]

let num_actions = List.length action_list

let tag_to_action (action : int) : t =
  if action >= num_actions + Var.max_num_vars
  then Construct (Arg (action - num_actions - Var.max_num_vars))
  else if action >= num_actions
  then Construct (Var (action - num_actions))
  else
    try List.nth action_list action
    with Failure _ | Invalid_argument _ ->
      raise (Failure "Invalid action index")

let action_to_tag (action : t) : int =
  let rec find x lst c =
    match lst with
    | [] -> raise (Failure "Invalid action")
    | hd :: tl -> if hd = x then c else find x tl (c + 1)
  in
  match action with
  | Construct (Var x) -> x + num_actions
  | Construct (Arg x) -> x + num_actions + Var.max_num_vars
  | _ -> find action action_list 0

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
