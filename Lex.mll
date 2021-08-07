{
open Parse
}

rule token = parse
  | [' ' '\r' '\n' '\t']+ { token lexbuf }
  | ['a'-'z' 'A'-'Z' '_' '+' '-' '*' '/' '%']+ as id { if id = "fn" then FN else IDENT id }
  | ['0'-'9']+ as i { match int_of_string i with exception Failure _ -> ERROR "invalid integer" | x -> INT x }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _   { ERROR "unexpected character" }
