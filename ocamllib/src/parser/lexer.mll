{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = 'x' digit+

rule read =
  parse
  | [' ' '\t' '\n'] { read lexbuf }
  | int as i{ INT (int_of_string i) }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "/"     { DIV }
  | "!"     { NOT }
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
  | "&&"    { AND }
  | "||"    { OR }
  | "?"     { HOLE }
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
  | "list"  { TLIST }
  | "map"   { MAP }
  | "fold"  { FOLD }
  | "filter" {FILTER }
  | "assert"{ ASSERT }
  | "equal" { LISTEQ }
  | "f"     { F }
  | "match" { MATCH }
  | "with"  { WITH }
  | "|"     { BAR }
  | "_"     { WILD }
  | "->"    { RIGHTARROW }
  | id as x { ID (int_of_string (String.sub x 1 (String.length x - 1))) }
  | eof     { EOF }
  | _       { raise (Failure "unknown token")}
