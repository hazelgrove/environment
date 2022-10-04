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
%token ASSERT
%token AND, OR
%token F

%{ 
    open Expr
    open Type
%}
%start <Expr.t> main

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
        | None -> Expr.make_node EHole
        | Some e -> e
    in
    Expr.make_node (ELet (x, e1, ebody))
    }
| LET x = identifiers args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> Expr.make_node (EFun(a, ty, e))
            | (a, ty) :: tl -> Expr.make_node (EFun(a, ty, resolve_fun tl e))
    in
    let ebody = 
        match e2 with
        | None -> Expr.make_node EHole
        | Some e -> e
    in
    Expr.make_node (ELet (x, resolve_fun args e1, ebody))
    }
| LET REC x = identifiers args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> Expr.make_node (EFun(a, ty, e))
            | (a, ty) :: tl -> Expr.make_node (EFun(a, ty, resolve_fun tl e))
    in
    let ebody = 
        match e2 with
        | None -> Expr.make_node EHole
        | Some e -> e
    in
    Expr.make_node (ELet (x, Expr.make_node (EFix (x, Type.make_node THole, resolve_fun args e1)), ebody))    
    }
| IF econd = expr THEN ethen = expr ELSE eelse = expr 
    {
    Expr.make_node (EIf (econd, ethen, eelse))
    }   
| FUN x = arg RIGHTARROW e = expr
    {
    let (a, ty) = x in
    Expr.make_node (EFun (a, ty, e))
    }
| LBRAC es = separated_list(SEMI, expr) RBRAC
    { let rec resolve_list es =
        match es with
            | hd :: [] -> Expr.make_node (EBinOp(hd, OpCons, Expr.make_node ENil))
            | hd :: tl -> Expr.make_node (EBinOp(hd, OpCons, resolve_list tl))
            | _ -> raise (Failure "Incorrect syntax")
    in
    resolve_list es
    }
| ASSERT LPAREN e = expr RPAREN
    { Expr.make_node (EAssert e) }

logicor:
| e = logicand
    { e }
| e = logicor OR e2 = logicand
    { Expr.make_node (EBinOp(e, OpOr, e2)) }

logicand:
| e = comp
    { e }
| e = logicand AND e2 = comp
    { Expr.make_node (EBinOp(e, OpAnd, e2)) }

comp: 
| e = lst 
    { e }
| e1 = lst LT e2 = lst
    { Expr.make_node (EBinOp (e1, OpLt, e2)) }
| e1 = lst LE e2 = lst
    { Expr.make_node (EBinOp (e1, OpLe, e2)) }
| e1 = lst GT e2 = lst
    { Expr.make_node (EBinOp (e1, OpGt, e2)) }
| e1 = lst GE e2 = lst
    { Expr.make_node (EBinOp (e1, OpGe, e2)) }
| e1 = lst EQ e2 = lst
    { Expr.make_node (EBinOp (e1, OpEq, e2)) }
| e1 = lst NE e2 = lst
    { Expr.make_node (EBinOp (e1, OpNe, e2)) }

lst:
| e = arith
    { e }
| e1 = arith CON e2 = lst
    { Expr.make_node (EBinOp (e1, OpCons, e2)) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { Expr.make_node (EBinOp (e1, OpPlus, e2)) }
| e1 = arith MINUS e2 = factor
    { Expr.make_node (EBinOp (e1, OpMinus, e2)) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { Expr.make_node (EBinOp (e1, OpTimes, e2)) }
| e1 = factor DIV e2 = app
    { Expr.make_node (EBinOp (e1, OpDiv, e2)) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { Expr.make_node (EBinOp (e1, OpAp, e2))}
| MINUS e = simple
    { Expr.make_node (EUnOp (OpNeg, e)) }

simple: 
| x = identifiers
    { Expr.make_node (EVar x) }
| TRUE
    { Expr.make_node (EBool true) }
| FALSE
    { Expr.make_node (EBool false) }
| n = INT
    { Expr.make_node (EInt n) }
| LBRAC RBRAC
    { Expr.make_node ENil }
| LPAREN e = expr RPAREN
    { e }
| LPAREN e1 = expr COMMA e2 = expr RPAREN
    { Expr.make_node (EPair (e1, e2)) }

arg: 
| x = identifiers
    { (x, Type.make_node THole) }
| LPAREN x = identifiers t = tyann RPAREN
    { (x, t) }
| LPAREN x = identifiers RPAREN
    { (x, Type.make_node THole) }

tyann: 
| OFTYPE t = ty 
    { t }

ty:
| t = ty_prod
    { t }
| t1 = ty_prod RIGHTARROW t2 = ty
    { Type.make_node (TArrow (t1, t2)) }

ty_prod:
| t = base_ty
    { t }
| t1 = base_ty TIMES t2 = ty_prod
    { Type.make_node (TProd (t1, t2)) }

base_ty:
| TINT
    { Type.make_node TInt }
| TBOOL
    { Type.make_node TBool }
| LPAREN t = ty RPAREN
    { t }

scope:
| IN e = expr 
    { e }

identifiers:
| x = ID
    { x }
| F
    { Var.starter_func }
