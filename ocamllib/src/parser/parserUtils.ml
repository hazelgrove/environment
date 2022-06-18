(* Functions that help parsing *)

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
      parse "let f (x : int) = x != 2 in f 1"
      = ELet
          ( "f",
            EFun ("x", TInt, EBinOp (EVar "x", OpNe, EInt 2)),
            EBinOp (EVar "f", OpAp, EInt 1) )

    let%test _ =
      parse "let f x (y : int) = x + y"
      = ELet
          ( "f",
            EFun
              ("x", THole, EFun ("y", TInt, EBinOp (EVar "x", OpPlus, EVar "y"))),
            EHole )

    let%test _ =
      parse
        "let rec fact (n : int) = if n = 0 then 1 else n * fact (n - 1) in \
         fact 4"
      = ELet
          ( "fact",
            EFix
              ( "fact",
                THole,
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
      parse "let x = true in if x then 2 else 3"
      = ELet ("x", EBool true, EIf (EVar "x", EInt 2, EInt 3))

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
