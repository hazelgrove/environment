%token <int> INT
%token <string> ID
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token LT LE GT GE EQ NE
%token CON
%token NEG
%token LET IN
%token IF THEN ELSE
%token FUN RIGHTARROW AP REC
%token LPAREN RPAREN LBRAC RBRAC
%token LIST
%token COMMA SEMI
%token EOF

%nonassoc IN RIGHTARROW
%left LT LE GT GE EQ NE
%right CON
%left PLUS MINUS
%left TIMES DIV
%nonassoc NEG
%left AP


%{ open Ast %}
%start <Ast.Expr.t> main

%%

main:
| e = expr EOF
    { e }

expr:
| i = INT
    { Expr.EInt i }
| TRUE
    { Expr.EBool true }
| FALSE
    { Expr.EBool false }
| x = ID 
    {Expr.EVar x}
| LPAREN es = separated_list(COMMA, e = expr { e }) RPAREN
    { 
    let rec resolve_tuple es =
        match es with
            | hd :: tl :: [] -> Expr.EPair(hd, tl)
            | hd :: tl -> Expr.EPair(hd, resolve_tuple tl)
            | _ -> raise (Failure "Incorrect syntax")
    in
    resolve_tuple es
    }
| LIST
    { Expr.ENil }
| LBRAC es = separated_list(SEMI, e = expr { e }) RBRAC
    { let rec resolve_list es =
        match es with
            | hd :: [] -> Expr.EBinOp(hd, OpCon, Expr.ENil)
            | hd :: tl -> Expr.EBinOp(hd, OpCon, resolve_list tl)
            | _ -> raise (Failure "Incorrect syntax")
    in
    resolve_list es 
    }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr e2 = expr %prec AP
    { Expr.EBinOp (e1, Expr.OpAp, e2) }
| e1 = expr PLUS e2 = expr
    { Expr.EBinOp (e1, Expr.OpPlus, e2) }
| e1 = expr MINUS e2 = expr
    { Expr.EBinOp (e1, Expr.OpMinus, e2) }
| e1 = expr TIMES e2 = expr
    { Expr.EBinOp (e1, Expr.OpTimes, e2) }
| e1 = expr DIV e2 = expr
    { Expr.EBinOp (e1, Expr.OpDiv, e2) }
| e1 = expr LT e2 = expr
    { Expr.EBinOp (e1, Expr.OpLt, e2) }
| e1 = expr LE e2 = expr
    { Expr.EBinOp (e1, Expr.OpLe, e2) }
| e1 = expr GT e2 = expr
    { Expr.EBinOp (e1, Expr.OpGt, e2) }
| e1 = expr GE e2 = expr
    { Expr.EBinOp (e1, Expr.OpGe, e2) }
| e1 = expr EQ e2 = expr
    { Expr.EBinOp (e1, Expr.OpEq, e2) }
| e1 = expr NE e2 = expr
    { Expr.EBinOp (e1, Expr.OpNe, e2) }
| e1 = expr CON e2 = expr
    { Expr.EBinOp (e1, Expr.OpCon, e2) }
| MINUS e = expr %prec NEG
    { Expr.EUnOp(Expr.OpNeg, e) }
| LET x = ID EQ e1 = expr IN e2 = expr
    { Expr.ELet(x, e1, e2) }
| IF b = expr THEN e1 = expr ELSE e2 = expr
    { Expr.EIf(b, e1, e2) }
| FUN x = ID RIGHTARROW e = expr
    { Expr.EFun(x, e) }
| LET REC x = ID args = ID+ EQ e1 = expr; e2 = option(e = scope { e })
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [arg] -> Expr.EFun(arg, e)
            | hd :: tl -> Expr.EFun(hd, resolve_fun tl e)
    in
        match e2 with
        | None -> Expr.ELet(x, Expr.EFix(x, resolve_fun args e1), Expr.EHole)
        | Some e -> Expr.ELet(x, Expr.EFix(x, resolve_fun args e1), e)
    }
| LET x = ID args = ID+ EQ e1 = expr; e2 = option(e = scope { e })
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [arg] -> Expr.EFun(arg, e)
            | hd :: tl -> Expr.EFun(hd, resolve_fun tl e)
    in
        match e2 with
        | None -> Expr.ELet(x, resolve_fun args e1, Expr.EHole)
        | Some e -> Expr.ELet(x, resolve_fun args e1, e)
    }

scope:
| IN e = expr 
    { e }