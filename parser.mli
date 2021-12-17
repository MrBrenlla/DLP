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
  | LIST
  | HEAD
  | TAIL
  | ISEMPTY
  | PROJECT
  | BOOL
  | NAT
  | STR
  | FST
  | SCN
  | LPAREN
  | RPAREN
  | LCORCH
  | RCORCH
  | LBRAC
  | RBRAC
  | COMA
  | DOT
  | EQ
  | AST
  | COLON
  | ARROW
  | EOF
  | INTV of (int)
  | STRINGV of (string)
  | STRING of (string)

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.variable
