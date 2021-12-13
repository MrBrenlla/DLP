
{
  open Parser;;
  exception Lexical_error;;
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "concat"    { CONCAT }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "Bool"      { BOOL }
  | "Nat"       { NAT }
  | "Str"       { STR }
  | "first"     { FST }
  | "second"    { SCN }
  | "list"      { LIST }
  | "head"      { HEAD }
  | "tail"      { TAIL }
  | "isempty"   { ISEMPTY }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '['         { LCORCH }
  | ']'         { RCORCH }
  | '{'         { LBRAC }
  | '}'         { RBRAC }
  | ','         { COMA }
  | '.'         { DOT }
  | '='         { EQ }
  | '*'         { AST }
  | ':'         { COLON }
  | "->"        { ARROW }
  | '"'[^'"']*'"'     { STRING (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2))}
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | eof         { EOF }
  | _           { raise Lexical_error }
