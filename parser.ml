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
  | BOOL
  | NAT
  | STR
  | FST
  | SCN
  | LPAREN
  | RPAREN
  | LCORCH
  | RCORCH
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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 43 "parser.ml"
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
  270 (* LIST *);
  271 (* HEAD *);
  272 (* TAIL *);
  273 (* BOOL *);
  274 (* NAT *);
  275 (* STR *);
  276 (* FST *);
  277 (* SCN *);
  278 (* LPAREN *);
  279 (* RPAREN *);
  280 (* LCORCH *);
  281 (* RCORCH *);
  282 (* COMA *);
  283 (* DOT *);
  284 (* EQ *);
  285 (* AST *);
  286 (* COLON *);
  287 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  288 (* INTV *);
  289 (* STRINGV *);
  290 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\006\000\006\000\004\000\004\000\004\000\004\000\
\007\000\007\000\007\000\007\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\002\000\003\000\002\000\002\000\
\002\000\003\000\005\000\002\000\003\000\001\000\001\000\001\000\
\001\000\001\000\003\000\001\000\001\000\003\000\003\000\002\000\
\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\022\000\023\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\026\000\037\000\000\000\000\000\
\008\000\000\000\024\000\000\000\009\000\012\000\013\000\000\000\
\000\000\000\000\016\000\015\000\010\000\011\000\000\000\020\000\
\000\000\000\000\001\000\017\000\000\000\000\000\014\000\000\000\
\000\000\018\000\000\000\028\000\000\000\021\000\000\000\034\000\
\035\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\027\000\033\000\005\000\000\000\
\000\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\022\000\023\000\024\000\060\000\025\000\054\000\061\000"

let yysindex = "\013\000\
\084\255\000\000\225\254\000\000\000\000\118\255\005\255\005\255\
\005\255\005\255\233\254\015\255\005\255\005\255\005\255\005\255\
\118\255\050\255\000\000\004\255\000\000\000\000\050\000\005\255\
\000\000\033\255\000\000\068\255\000\000\000\000\000\000\005\255\
\049\255\048\255\000\000\000\000\000\000\000\000\023\255\000\000\
\030\255\118\255\000\000\000\000\243\254\118\255\000\000\118\255\
\243\254\000\000\118\255\000\000\118\255\000\000\089\000\000\000\
\000\000\000\000\243\254\255\254\059\255\092\255\090\255\006\255\
\086\255\030\255\000\000\131\255\000\000\118\255\243\254\243\254\
\118\255\118\255\118\255\000\000\000\000\000\000\000\000\001\255\
\001\255\000\000\000\000\094\255\118\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\130\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\053\255\
\074\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\228\255\009\000\044\000\000\000"

let yytablesize = 291
let yytable = "\028\000\
\024\000\026\000\003\000\056\000\057\000\058\000\004\000\005\000\
\059\000\033\000\039\000\041\000\069\000\001\000\069\000\029\000\
\030\000\031\000\032\000\069\000\064\000\035\000\036\000\037\000\
\038\000\070\000\017\000\071\000\018\000\071\000\068\000\042\000\
\044\000\075\000\071\000\055\000\019\000\027\000\021\000\062\000\
\047\000\063\000\080\000\081\000\065\000\050\000\066\000\034\000\
\051\000\043\000\003\000\004\000\005\000\006\000\052\000\053\000\
\007\000\008\000\009\000\010\000\011\000\012\000\045\000\079\000\
\013\000\014\000\082\000\083\000\084\000\015\000\016\000\017\000\
\046\000\018\000\040\000\031\000\048\000\049\000\086\000\031\000\
\031\000\019\000\027\000\021\000\003\000\004\000\005\000\006\000\
\067\000\072\000\007\000\008\000\009\000\010\000\011\000\012\000\
\030\000\073\000\013\000\014\000\030\000\030\000\074\000\015\000\
\016\000\017\000\085\000\018\000\076\000\077\000\000\000\000\000\
\000\000\000\000\000\000\019\000\020\000\021\000\003\000\004\000\
\005\000\006\000\000\000\000\000\007\000\008\000\009\000\010\000\
\011\000\012\000\000\000\000\000\013\000\014\000\000\000\000\000\
\000\000\015\000\016\000\017\000\000\000\018\000\000\000\029\000\
\069\000\000\000\000\000\000\000\000\000\019\000\027\000\021\000\
\029\000\078\000\000\000\000\000\029\000\029\000\029\000\071\000\
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
\000\000\000\000\024\000\024\000\000\000\000\000\000\000\003\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\
\024\000\003\000\000\000\003\000\003\000\000\000\000\000\000\000\
\024\000\024\000\024\000"

let yycheck = "\006\000\
\000\000\033\001\000\000\017\001\018\001\019\001\002\001\003\001\
\022\001\033\001\017\000\018\000\014\001\001\000\014\001\007\000\
\008\000\009\000\010\000\014\001\049\000\013\000\014\000\015\000\
\016\000\027\001\022\001\029\001\024\001\029\001\059\000\028\001\
\024\000\028\001\029\001\042\000\032\001\033\001\034\001\046\000\
\032\000\048\000\071\000\072\000\051\000\023\001\053\000\033\001\
\026\001\000\000\001\001\002\001\003\001\004\001\025\001\026\001\
\007\001\008\001\009\001\010\001\011\001\012\001\030\001\070\000\
\015\001\016\001\073\000\074\000\075\000\020\001\021\001\022\001\
\005\001\024\001\025\001\023\001\028\001\030\001\085\000\027\001\
\028\001\032\001\033\001\034\001\001\001\002\001\003\001\004\001\
\000\000\031\001\007\001\008\001\009\001\010\001\011\001\012\001\
\023\001\006\001\015\001\016\001\027\001\028\001\013\001\020\001\
\021\001\022\001\013\001\024\001\023\001\066\000\255\255\255\255\
\255\255\255\255\255\255\032\001\033\001\034\001\001\001\002\001\
\003\001\004\001\255\255\255\255\007\001\008\001\009\001\010\001\
\011\001\012\001\255\255\255\255\015\001\016\001\255\255\255\255\
\255\255\020\001\021\001\022\001\255\255\024\001\255\255\014\001\
\014\001\255\255\255\255\255\255\255\255\032\001\033\001\034\001\
\023\001\023\001\255\255\255\255\027\001\028\001\029\001\029\001\
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
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\255\255\255\255\255\255\255\255\255\255\022\001\255\255\
\024\001\023\001\255\255\025\001\026\001\255\255\255\255\255\255\
\032\001\033\001\034\001"

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
  LIST\000\
  HEAD\000\
  TAIL\000\
  BOOL\000\
  NAT\000\
  STR\000\
  FST\000\
  SCN\000\
  LPAREN\000\
  RPAREN\000\
  LCORCH\000\
  RCORCH\000\
  COMA\000\
  DOT\000\
  EQ\000\
  AST\000\
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
# 51 "parser.mly"
      ( VarValue _1 )
# 271 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 53 "parser.mly"
      (VarAsignation (_1,_3))
# 279 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 57 "parser.mly"
      ( _1 )
# 286 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 59 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 295 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 61 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 304 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 63 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 313 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 65 "parser.mly"
      (TmLetIn (_2, TmFix(TmAbs(_2,_4,_6)),_8) )
# 323 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 70 "parser.mly"
      ( _1 )
# 330 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( TmSucc _2 )
# 337 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 74 "parser.mly"
      ( TmFirst _2)
# 344 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 76 "parser.mly"
      (TmSecond _2)
# 351 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( TmPred _2 )
# 358 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 80 "parser.mly"
      ( TmIsZero _2 )
# 365 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 82 "parser.mly"
      (TmConcat (_2,_3))
# 373 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 84 "parser.mly"
      (TmTail _2)
# 380 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 86 "parser.mly"
      (TmHead _2)
# 387 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( TmApp (_1, _2) )
# 395 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 92 "parser.mly"
      ( _2 )
# 402 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 94 "parser.mly"
    ( TmPair(_2,_4))
# 410 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
      (TmList [])
# 416 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lista) in
    Obj.repr(
# 98 "parser.mly"
    (TmList (_2::_3))
# 424 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
      ( TmTrue )
# 430 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
      ( TmFalse )
# 436 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
      ( TmVar _1 )
# 443 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 106 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 453 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
      ( TmString _1)
# 460 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lista) in
    Obj.repr(
# 115 "parser.mly"
      (_2::_3)
# 468 "parser.ml"
               : 'lista))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
      ([])
# 474 "parser.ml"
               : 'lista))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 121 "parser.mly"
      ( _1 )
# 481 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 123 "parser.mly"
      ( TyArr (_1, _3) )
# 489 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 125 "parser.mly"
      ( TyPair (_1,_3))
# 497 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 127 "parser.mly"
      ( TyList _1)
# 504 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 131 "parser.mly"
      ( _2 )
# 511 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
      ( TyBool )
# 517 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
      ( TyNat )
# 523 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "parser.mly"
      ( TyStr )
# 529 "parser.ml"
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
