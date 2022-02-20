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
%token LPAREN RPAREN
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
| b = TRUE
    { Expr.EBool true }
| b = FALSE
    { Expr.EBool false }
| x = ID {Expr.EVar x}
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
| LET REC x = ID args = ID+ EQ e1 = expr IN e2 = expr
    {
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [arg] -> Expr.EFun(arg, e)
            | hd :: tl -> Expr.EFun(hd, resolve_fun tl e)
    in
        Expr.ELet(x, Expr.EFix(x, resolve_fun args e1), e2)
    }
| LET x = ID args = ID+ EQ e1 = expr IN e2 = expr
    { 
    let rec resolve_fun args e = 
        match args with
            | [] -> raise (Failure "Incorrect syntax")
            | [arg] -> Expr.EFun(arg, e)
            | hd :: tl -> Expr.EFun(hd, resolve_fun tl e)
    in
        Expr.ELet(x, resolve_fun args e1, e2)
    }