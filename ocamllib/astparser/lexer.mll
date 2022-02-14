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
  | white   { read lexbuf }
  | "+"     { PLUS }
  | "-"     { MINUS }
  | "*"     { TIMES }
  | "!="    { NE }
  | "<="    { LE }
  | ">="    { GE }
  | "="     { EQ }
  | "<"     { LT }
  | ">"     { GT }
  | "::"    { CON }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "true"  { TRUE }
  | "false" { FALSE }
  | "let"   { LET }
  | "in"    { IN }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "fun"   { FUN }
  | "->"    { RIGHTARROW }
  | id      { ID (Lexing.lexeme lexbuf) }
  | int     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof     { EOF }