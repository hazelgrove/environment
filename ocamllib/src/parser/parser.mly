%token <int> INT
%token <int> ID
%token TINT TBOOL TLIST
%token TRUE FALSE
%token IF THEN ELSE
%token FUN REC LET OFTYPE IN RIGHTARROW
%token MAP FILTER FOLD LISTEQ
%token COMMA SEMI
%token PLUS MINUS TIMES DIV
%token LT LE GT GE EQ NE
%token CON
%token LPAREN RPAREN LBRAC RBRAC
%token EOF
%token ASSERT
%token AND, OR, NOT
%token F
%token MATCH WITH BAR WILD
%token HOLE

%{ 
    open Expr
%}
%start <Expr.p_t> main

%%

main:
| e = expr EOF
    { e }

expr:
| e = logicor
    { e }
| LET x = identifiers EQ e1 = expr e2 = option(e = scope { e })
    { 
    let ebody = 
        match e2 with
        | None -> Expr.Hole
        | Some e -> e
    in
    Expr.Let (x, e1, ebody)
    }
| LET x = identifiers args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> Expr.Fun(a, ty, e)
            | (a, ty) :: tl -> Expr.Fun(a, ty, resolve_fun tl e)
    in
    let ebody = 
        match e2 with
        | None -> Expr.Hole
        | Some e -> e
    in
    Expr.Let (x, resolve_fun args e1, ebody)
    }
| LET REC x = identifiers args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> Expr.Fun(a, ty, e)
            | (a, ty) :: tl -> Expr.Fun(a, ty, resolve_fun tl e)
    in
    let ebody = 
        match e2 with
        | None -> Expr.Hole
        | Some e -> e
    in
    Expr.Let (x, Fix (x, Type.Hole, resolve_fun args e1), ebody)
    }
| IF econd = expr THEN ethen = expr ELSE eelse = expr 
    {
    Expr.If (econd, ethen, eelse)
    }   
| FUN x = arg RIGHTARROW e = expr
    {
    let (a, ty) = x in
    Expr.Fun (a, ty, e)
    }
| MATCH escrut = expr WITH rules = rule+
    {
    let rec resolve_rules rules = 
        match rules with
        | [] -> raise (Failure "Incorrect syntax")
        | [(p, e)] -> Expr.Match(escrut, (p, e), (Pattern.Wild, Expr.Hole))
        | [(p1, e1); (p2, e2)] -> Expr.Match(escrut, (p1, e1), (p2, e2))
        | (p1, e1) :: tl -> Expr.Match(escrut, (p1, e1), (Pattern.Wild, resolve_rules tl))
    in
    resolve_rules rules
    }
| ASSERT LPAREN e = expr RPAREN
    { Expr.Assert e }
| LBRAC es = separated_list(SEMI, expr) RBRAC
    { let rec resolve_list es =
        match es with
            | hd :: [] -> Expr.BinOp(hd, OpCons, Const Nil)
            | hd :: tl -> Expr.BinOp(hd, OpCons, resolve_list tl)
            | _ -> raise (Failure "Incorrect syntax")
    in
    resolve_list es
    }

logicor:
| e = logicand
    { e }
| e = logicor OR e2 = logicand
    { Expr.BinOp(e, OpOr, e2) }

logicand:
| e = logicnot
    { e }
| e = logicand AND e2 = comp
    { Expr.BinOp(e, OpAnd, e2) }

logicnot:
| e = comp
    { e }
| NOT e = comp
    { Expr.UnOp(OpNot, e) }

comp: 
| e = lst 
    { e }
| e1 = lst LT e2 = lst
    { Expr.BinOp (e1, OpLt, e2) }
| e1 = lst LE e2 = lst
    { Expr.BinOp (e1, OpLe, e2) }
| e1 = lst GT e2 = lst
    { Expr.BinOp (e1, OpGt, e2) }
| e1 = lst GE e2 = lst
    { Expr.BinOp (e1, OpGe, e2) }
| e1 = lst EQ e2 = lst
    { Expr.BinOp (e1, OpEq, e2) }
| e1 = lst NE e2 = lst
    { Expr.BinOp (e1, OpNe, e2) }

lst:
| e = arith
    { e }
| e1 = arith CON e2 = lst
    { Expr.BinOp (e1, OpCons, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { Expr.BinOp (e1, OpPlus, e2) }
| e1 = arith MINUS e2 = factor
    { Expr.BinOp (e1, OpMinus, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { Expr.BinOp (e1, OpTimes, e2) }
| e1 = factor DIV e2 = app
    { Expr.BinOp (e1, OpDiv, e2) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Expr.BinOp (e1, OpAp, e2) }
| MAP e1 = app e2 = simple 
    { Expr.Map (e1, e2) }
| FILTER e1 = app e2 = simple 
    { Expr.Filter (e1, e2) }
| FOLD e1 = app e2 = simple e3 = simple
    { Expr.Fold (e1, e2, e3) }
| LISTEQ e1 = app e2 = simple 
    { Expr.ListEq (e1, e2) }
| MINUS e = simple
    { Expr.UnOp (OpNeg, e) }

simple: 
| x = ID
    { Expr.Var x }
| x = identifiers
    { Var x }
| TRUE
    { Expr.Const (Bool true) }
| FALSE
    { Expr.Const (Bool false) }
| n = INT
    { Expr.Const (Int n) }
| LBRAC RBRAC
    { Expr.Const Nil }
| LPAREN e = expr RPAREN
    { e }
| LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Expr.Pair (e1, e2) }
| HOLE
    { Expr.Hole }

arg: 
| x = identifiers
    { (x, Type.Hole) }
| LPAREN x = identifiers t = tyann RPAREN
    { (x, t) }
| LPAREN x = identifiers RPAREN
    { (x, Type.Hole) }

rule:
| BAR p = pattern RIGHTARROW e = expr
    { (p, e) }

pattern:
| LPAREN p = pattern RPAREN
    { p }
| p1 = pattern CON p2 = pattern
    { Pattern.Cons (p1, p2) }
| x = ID
    { Pattern.Var x }
| TRUE
    { Pattern.Const (Bool true) }
| FALSE
    { Pattern.Const (Bool false) }
| n = INT
    { Pattern.Const (Int n) }
| LBRAC RBRAC
    { Pattern.Const Nil }
| WILD
    { Pattern.Wild }

tyann: 
| OFTYPE t = ty 
    { t }

ty:
| t = ty_prod
    { t }
| t1 = ty_prod RIGHTARROW t2 = ty
    { Type.Arrow (t1, t2) }

ty_prod:
| t = ty_list
    { t }
| t1 = ty_list TIMES t2 = ty_prod
    { Type.Prod (t1, t2) }

ty_list:
| t = base_ty
    { t }
| t1 = base_ty TLIST
    { Type.List t1 }

base_ty:
| TINT
    { Type.Int }
| TBOOL
    { Type.Bool }
| LPAREN t = ty RPAREN
    { t }
| HOLE
    { Type.Hole }

scope:
| IN e = expr 
    { e }

identifiers:
| x = ID
    { x }
| F
    { Var.starter_func }
