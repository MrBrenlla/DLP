type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | CONCAT
  | LET
  | LETREC
  | IN
  | BOOL
  | NAT
  | STR
  | FST
  | SCN
  | LPAREN
  | RPAREN
  | COMA
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STRING of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.variable
