%token <int> INT
%token <int> ID
%token TINT TBOOL
%token TRUE FALSE
%token IF THEN ELSE
%token FUN REC LET OFTYPE IN RIGHTARROW
%token COMMA SEMI
%token PLUS MINUS TIMES DIV
%token LT LE GT GE EQ NE
%token CON
%token LPAREN RPAREN LBRAC RBRAC
%token EOF
%token MATCH WITH BAR WILD

%{ 
    open Expr
%}
%start <Expr.p_t> main

%%

main:
| e = expr EOF
    { e }

expr:
| e = boolean
    { e }
| LET x = ID  EQ e1 = expr e2 = option(e = scope { e })
    { 
    let ebody = 
        match e2 with
        | None -> Expr.Hole
        | Some e -> e
    in
    Expr.Let (x, e1, ebody)
    }
| LET x = ID args = arg+ EQ e1 = expr; e2 = option(scope)
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
| LET REC x = ID args = arg+ EQ e1 = expr; e2 = option(scope)
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
| LBRAC es = separated_list(SEMI, expr) RBRAC
    { 
    Expr.List es
    }
| MATCH escrut = expr WITH rules = rule+
    {
        Expr.Match (escrut, rules)
    }

boolean: 
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
| MINUS e = simple
    { Expr.UnOp (OpNeg, e) }

simple: 
| x = ID
    { Expr.Var x }
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

arg: 
| x = ID
    { (x, Type.Hole) }
| LPAREN x = ID t = tyann RPAREN
    { (x, t) }
| LPAREN x = ID RPAREN
    { (x, Type.Hole) }

rule:
| BAR p = pattern RIGHTARROW e = expr
    { (p, e) }

pattern:
| LPAREN p = pattern RPAREN
    { p }
| p1 = pattern CON p2 = pattern
    { Pattern.List (p1, p2) }
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
| t = base_ty
    { t }
| t1 = base_ty TIMES t2 = ty_prod
    { Type.Prod (t1, t2) }

base_ty:
| TINT
    { Type.Int }
| TBOOL
    { Type.Bool }
| LPAREN t = ty RPAREN
    { t }

scope:
| IN e = expr 
    { e }
