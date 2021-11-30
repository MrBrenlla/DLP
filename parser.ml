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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 37 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* CONCAT *);
  267 (* LET *);
  268 (* LETREC *);
  269 (* IN *);
  270 (* BOOL *);
  271 (* NAT *);
  272 (* STR *);
  273 (* FST *);
  274 (* SCN *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* COMA *);
  278 (* DOT *);
  279 (* EQ *);
  280 (* COLON *);
  281 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  282 (* INTV *);
  283 (* STRINGV *);
  284 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\004\000\004\000\
\006\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\002\000\003\000\002\000\003\000\
\005\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\018\000\019\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\021\000\
\000\000\022\000\029\000\000\000\000\000\008\000\000\000\020\000\
\000\000\009\000\012\000\013\000\000\000\000\000\000\000\010\000\
\011\000\000\000\000\000\001\000\015\000\000\000\000\000\014\000\
\000\000\000\000\016\000\000\000\000\000\026\000\027\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\017\000\025\000\
\005\000\024\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\019\000\020\000\021\000\050\000\022\000\051\000"

let yysindex = "\003\000\
\004\255\000\000\248\254\000\000\000\000\058\255\015\255\015\255\
\015\255\015\255\253\254\255\254\015\255\015\255\058\255\000\000\
\017\255\000\000\000\000\039\000\015\255\000\000\023\255\000\000\
\043\255\000\000\000\000\000\000\015\255\027\255\029\255\000\000\
\000\000\016\255\058\255\000\000\000\000\030\255\058\255\000\000\
\058\255\030\255\000\000\058\255\051\000\000\000\000\000\000\000\
\030\255\035\255\033\255\057\255\059\255\048\255\053\255\000\000\
\054\255\058\255\030\255\058\255\058\255\058\255\000\000\000\000\
\000\000\000\000\000\000\000\000\065\255\058\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\005\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\217\255\080\000\000\000"

let yytablesize = 285
let yytable = "\025\000\
\020\000\003\000\054\000\001\000\003\000\004\000\005\000\006\000\
\034\000\057\000\007\000\008\000\009\000\010\000\011\000\012\000\
\004\000\005\000\023\000\066\000\013\000\014\000\015\000\030\000\
\023\000\031\000\023\000\023\000\045\000\016\000\017\000\018\000\
\052\000\015\000\053\000\043\000\044\000\055\000\036\000\035\000\
\016\000\024\000\018\000\046\000\047\000\048\000\038\000\039\000\
\049\000\041\000\056\000\065\000\042\000\067\000\068\000\069\000\
\058\000\059\000\003\000\004\000\005\000\006\000\060\000\071\000\
\007\000\008\000\009\000\010\000\011\000\012\000\062\000\061\000\
\063\000\064\000\013\000\014\000\015\000\070\000\000\000\000\000\
\000\000\000\000\000\000\016\000\024\000\018\000\026\000\027\000\
\028\000\029\000\000\000\000\000\032\000\033\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\020\000\020\000\000\000\000\000\003\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\000\000\000\000\020\000\000\000\003\000\003\000\000\000\
\000\000\000\000\020\000\020\000\020\000"

let yycheck = "\006\000\
\000\000\000\000\042\000\001\000\001\001\002\001\003\001\004\001\
\015\000\049\000\007\001\008\001\009\001\010\001\011\001\012\001\
\002\001\003\001\027\001\059\000\017\001\018\001\019\001\027\001\
\020\001\027\001\022\001\023\001\035\000\026\001\027\001\028\001\
\039\000\019\001\041\000\020\001\021\001\044\000\000\000\023\001\
\026\001\027\001\028\001\014\001\015\001\016\001\024\001\005\001\
\019\001\023\001\000\000\058\000\024\001\060\000\061\000\062\000\
\022\001\025\001\001\001\002\001\003\001\004\001\006\001\070\000\
\007\001\008\001\009\001\010\001\011\001\012\001\023\001\013\001\
\020\001\020\001\017\001\018\001\019\001\013\001\255\255\255\255\
\255\255\255\255\255\255\026\001\027\001\028\001\007\000\008\000\
\009\000\010\000\255\255\255\255\013\000\014\000\255\255\255\255\
\255\255\255\255\255\255\255\255\021\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\029\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\255\255\
\255\255\255\255\255\255\019\001\255\255\020\001\021\001\255\255\
\255\255\255\255\026\001\027\001\028\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  CONCAT\000\
  LET\000\
  LETREC\000\
  IN\000\
  BOOL\000\
  NAT\000\
  STR\000\
  FST\000\
  SCN\000\
  LPAREN\000\
  RPAREN\000\
  COMA\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  "

let yynames_block = "\
  INTV\000\
  STRINGV\000\
  STRING\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 44 "parser.mly"
      ( VarValue _1 )
# 243 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 46 "parser.mly"
      (VarAsignation (_1,_3))
# 251 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 50 "parser.mly"
      ( _1 )
# 258 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 52 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 267 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 276 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 285 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 58 "parser.mly"
      (TmLetIn (_2, TmFix(TmAbs(_2,_4,_6)),_8) )
# 295 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 63 "parser.mly"
      ( _1 )
# 302 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
      ( TmSucc _2 )
# 309 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( TmFirst _2)
# 316 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      (TmSecond _2)
# 323 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmPred _2 )
# 330 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( TmIsZero _2 )
# 337 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      (TmConcat (_2,_3))
# 345 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmApp (_1, _2) )
# 353 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 81 "parser.mly"
      ( _2 )
# 360 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 83 "parser.mly"
    ( TmPair(_2,_4))
# 368 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
      ( TmTrue )
# 374 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
      ( TmFalse )
# 380 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser.mly"
      ( TmVar _1 )
# 387 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 397 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
      ( TmString _1)
# 404 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 100 "parser.mly"
      ( _1 )
# 411 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 102 "parser.mly"
      ( TyArr (_1, _3) )
# 419 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 106 "parser.mly"
      ( _2 )
# 426 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
      ( TyBool )
# 432 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
      ( TyNat )
# 438 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
      ( TyStr )
# 444 "parser.ml"
               : 'atomicTy))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.variable)
