{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_')*

rule read =
  parse
  | [' ' '\t' '\n'] { read lexbuf }
  | int as i{ INT (int_of_string i) }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "/"     { DIV }
  | "!="    { NE }
  | "<="    { LE }
  | ">="    { GE }
  | "="     { EQ }
  | "<"     { LT }
  | ">"     { GT }
  | "::"    { CON }
  | ":"     { OFTYPE }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | ","     { COMMA }
  | ";"     { SEMI }
  | "["     { LBRAC }
  | "]"     { RBRAC }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "let"   { LET }
  | "in"    { IN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fun"   { FUN }
  | "rec"   { REC }
  | "int"   { TINT }
  | "bool"  { TBOOL }
  | "->"    { RIGHTARROW }
  | id as i { ID (i) }
  | eof     { EOF }
  | _       { raise (Failure "unknown token")}
