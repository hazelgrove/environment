%token <int> INT
%token <string> ID
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
| e = boolean
    { e }
| LET x = ID  EQ e1 = expr e2 = option(e = scope { e })
    { 
    let ebody = 
        match e2 with
        | None -> EHole
        | Some e -> e
    in
    ELet (x, e1, ebody)
    }
| LET x = ID args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> EFun(a, ty, e)
            | (a, ty) :: tl -> EFun(a, ty, resolve_fun tl e)
    in
    let ebody = 
        match e2 with
        | None -> EHole
        | Some e -> e
    in
    ELet (x, resolve_fun args e1, ebody)
    }
| LET REC x = ID args = arg+ EQ e1 = expr; e2 = option(scope)
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [(a, ty)] -> EFun(a, ty, e)
            | (a, ty) :: tl -> EFun(a, ty, resolve_fun tl e)
    in
    let ebody = 
        match e2 with
        | None -> Expr.EHole
        | Some e -> e
    in
    ELet (x, EFix (x, THole, resolve_fun args e1), ebody)
    }
| IF econd = expr THEN ethen = expr ELSE eelse = expr 
    {
    EIf (econd, ethen, eelse)
    }   
| FUN x = arg RIGHTARROW e = expr
    {
    let (a, ty) = x in
    EFun (a, ty, e)
    }
| LBRAC es = separated_list(SEMI, expr) RBRAC
    { let rec resolve_list es =
        match es with
            | hd :: [] -> EBinOp(hd, OpCons, ENil)
            | hd :: tl -> EBinOp(hd, OpCons, resolve_list tl)
            | _ -> raise (Failure "Incorrect syntax")
    in
    resolve_list es 
    }

boolean: 
| e = lst 
    { e }
| e1 = lst LT e2 = lst
    { EBinOp (e1, OpLt, e2) }
| e1 = lst LE e2 = lst
    { EBinOp (e1, OpLe, e2) }
| e1 = lst GT e2 = lst
    { EBinOp (e1, OpGt, e2) }
| e1 = lst GE e2 = lst
    { EBinOp (e1, OpGe, e2) }
| e1 = lst EQ e2 = lst
    { EBinOp (e1, OpEq, e2) }
| e1 = lst NE e2 = lst
    { EBinOp (e1, OpNe, e2) }

lst:
| e = arith
    { e }
| e1 = arith CON e2 = lst
    { EBinOp (e1, OpCons, e2) }

arith:
| e = factor
    { e }
| e1 = arith PLUS e2 = factor
    { EBinOp (e1, OpPlus, e2) }
| e1 = arith MINUS e2 = factor
    { EBinOp (e1, OpMinus, e2) }

factor:
| e = app
    { e }
| e1 = factor TIMES e2 = app
    { EBinOp (e1, OpTimes, e2) }
| e1 = factor DIV e2 = app
    { EBinOp (e1, OpDiv, e2) }

app:
| e = simple
    { e }
| e1 = app e2 = simple
    { EBinOp (e1, OpAp, e2)}
| MINUS e = simple
    { EUnOp (OpNeg, e) }

simple: 
| x = ID
    { EVar x }
| TRUE
    { EBool true }
| FALSE
    { EBool false }
| n = INT
    { EInt n }
| LBRAC RBRAC
    { ENil }
| LPAREN e = expr RPAREN
    { e }
| LPAREN e1 = expr COMMA e2 = expr RPAREN
    { EPair (e1, e2) }

arg: 
| x = ID
    { (x, THole) }
| LPAREN x = ID t = tyann RPAREN
    { (x, t) }
| LPAREN x = ID RPAREN
    { (x, THole) }

tyann: 
| OFTYPE t = ty 
    { t }

ty:
| t = ty_prod
    { t }
| t1 = ty_prod RIGHTARROW t2 = ty
    { TArrow (t1, t2) }

ty_prod:
| t = base_ty
    { t }
| t1 = base_ty TIMES t2 = ty_prod
    { TProd (t1, t2) }

base_ty:
| TINT
    { TInt }
| TBOOL
    { TBool }
| LPAREN t = ty RPAREN
    { t }

scope:
| IN e = expr 
    { e }
