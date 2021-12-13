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

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 46 "parser.ml"
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
  273 (* ISEMPTY *);
  274 (* BOOL *);
  275 (* NAT *);
  276 (* STR *);
  277 (* FST *);
  278 (* SCN *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* LCORCH *);
  282 (* RCORCH *);
  283 (* LBRAC *);
  284 (* RBRAC *);
  285 (* COMA *);
  286 (* DOT *);
  287 (* EQ *);
  288 (* AST *);
  289 (* COLON *);
  290 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  291 (* INTV *);
  292 (* STRINGV *);
  293 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\006\000\006\000\007\000\
\007\000\004\000\004\000\004\000\004\000\004\000\004\000\009\000\
\009\000\008\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\002\000\002\000\003\000\002\000\002\000\
\002\000\002\000\003\000\005\000\002\000\003\000\002\000\005\000\
\001\000\001\000\001\000\001\000\001\000\003\000\001\000\005\000\
\001\000\001\000\003\000\003\000\002\000\002\000\005\000\005\000\
\001\000\003\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\029\000\046\000\
\000\000\000\000\008\000\000\000\027\000\000\000\009\000\012\000\
\013\000\000\000\000\000\000\000\016\000\015\000\017\000\010\000\
\011\000\000\000\021\000\000\000\023\000\000\000\000\000\001\000\
\018\000\000\000\000\000\014\000\000\000\000\000\019\000\000\000\
\031\000\000\000\022\000\000\000\000\000\043\000\044\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\002\000\000\000\038\000\000\000\037\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\030\000\033\000\
\000\000\024\000\042\000\000\000\005\000\000\000\000\000\004\000\
\006\000\000\000\000\000\000\000\000\000\000\000\041\000\000\000\
\039\000\007\000\000\000\000\000\032\000\000\000\000\000\040\000"

let yydgoto = "\002\000\
\024\000\025\000\026\000\067\000\027\000\059\000\090\000\068\000\
\105\000"

let yysindex = "\009\000\
\119\255\000\000\000\255\000\000\000\000\156\255\003\255\003\255\
\003\255\003\255\010\255\017\255\003\255\003\255\003\255\003\255\
\003\255\156\255\082\255\236\254\000\000\254\254\000\000\000\000\
\059\000\003\255\000\000\042\255\000\000\071\255\000\000\000\000\
\000\000\003\255\050\255\049\255\000\000\000\000\000\000\000\000\
\000\000\020\255\000\000\241\254\000\000\056\255\156\255\000\000\
\000\000\167\255\156\255\000\000\156\255\167\255\000\000\156\255\
\000\000\156\255\000\000\156\255\100\000\000\000\000\000\000\000\
\167\255\007\255\244\254\067\255\096\255\097\255\034\255\087\255\
\241\254\044\255\000\000\092\255\000\000\079\255\000\000\156\255\
\167\255\167\255\156\255\156\255\156\255\000\000\000\000\000\000\
\077\255\000\000\000\000\167\255\000\000\251\254\251\254\000\000\
\000\000\101\255\084\255\005\255\156\255\156\255\000\000\089\255\
\000\000\000\000\044\255\099\255\000\000\167\255\005\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\249\254\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\255\121\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\206\255\054\000\060\000\030\000\000\000\
\027\000"

let yytablesize = 294
let yytable = "\030\000\
\027\000\079\000\003\000\071\000\004\000\005\000\034\000\045\000\
\079\000\001\000\057\000\042\000\044\000\058\000\076\000\046\000\
\034\000\080\000\079\000\081\000\034\000\034\000\034\000\034\000\
\034\000\018\000\081\000\019\000\047\000\020\000\094\000\095\000\
\103\000\104\000\077\000\028\000\081\000\021\000\029\000\023\000\
\061\000\100\000\078\000\055\000\069\000\035\000\070\000\079\000\
\056\000\072\000\036\000\073\000\036\000\074\000\036\000\036\000\
\036\000\036\000\048\000\111\000\031\000\032\000\033\000\034\000\
\085\000\081\000\037\000\038\000\039\000\040\000\041\000\088\000\
\089\000\093\000\050\000\051\000\096\000\097\000\098\000\049\000\
\053\000\054\000\003\000\004\000\005\000\006\000\060\000\052\000\
\007\000\008\000\009\000\010\000\011\000\012\000\106\000\107\000\
\013\000\014\000\015\000\075\000\082\000\083\000\016\000\017\000\
\018\000\079\000\019\000\043\000\020\000\084\000\086\000\092\000\
\099\000\101\000\102\000\091\000\021\000\029\000\023\000\003\000\
\004\000\005\000\006\000\081\000\108\000\007\000\008\000\009\000\
\010\000\011\000\012\000\110\000\087\000\013\000\014\000\015\000\
\109\000\112\000\000\000\016\000\017\000\018\000\000\000\019\000\
\035\000\020\000\000\000\000\000\035\000\035\000\035\000\035\000\
\000\000\021\000\022\000\023\000\003\000\004\000\005\000\006\000\
\000\000\000\000\007\000\008\000\009\000\010\000\011\000\012\000\
\000\000\000\000\013\000\014\000\015\000\000\000\000\000\000\000\
\016\000\017\000\018\000\000\000\019\000\000\000\020\000\000\000\
\062\000\063\000\064\000\000\000\000\000\065\000\021\000\029\000\
\023\000\066\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\027\000\027\000\000\000\000\000\000\000\003\000\
\003\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\000\000\027\000\003\000\027\000\003\000\000\000\003\000\003\000\
\000\000\000\000\000\000\027\000\027\000\027\000"

let yycheck = "\006\000\
\000\000\014\001\000\000\054\000\002\001\003\001\014\001\028\001\
\014\001\001\000\026\001\018\000\019\000\029\001\065\000\036\001\
\024\001\030\001\014\001\032\001\028\001\029\001\030\001\031\001\
\032\001\023\001\032\001\025\001\031\001\027\001\081\000\082\000\
\028\001\029\001\028\001\036\001\032\001\035\001\036\001\037\001\
\047\000\092\000\036\001\024\001\051\000\036\001\053\000\014\001\
\029\001\056\000\024\001\058\000\036\001\060\000\028\001\029\001\
\030\001\031\001\000\000\110\000\007\000\008\000\009\000\010\000\
\031\001\032\001\013\000\014\000\015\000\016\000\017\000\028\001\
\029\001\080\000\033\001\005\001\083\000\084\000\085\000\026\000\
\031\001\033\001\001\001\002\001\003\001\004\001\031\001\034\000\
\007\001\008\001\009\001\010\001\011\001\012\001\101\000\102\000\
\015\001\016\001\017\001\000\000\034\001\006\001\021\001\022\001\
\023\001\014\001\025\001\026\001\027\001\013\001\024\001\033\001\
\036\001\013\001\031\001\024\001\035\001\036\001\037\001\001\001\
\002\001\003\001\004\001\032\001\036\001\007\001\008\001\009\001\
\010\001\011\001\012\001\033\001\073\000\015\001\016\001\017\001\
\107\000\111\000\255\255\021\001\022\001\023\001\255\255\025\001\
\024\001\027\001\255\255\255\255\028\001\029\001\030\001\031\001\
\255\255\035\001\036\001\037\001\001\001\002\001\003\001\004\001\
\255\255\255\255\007\001\008\001\009\001\010\001\011\001\012\001\
\255\255\255\255\015\001\016\001\017\001\255\255\255\255\255\255\
\021\001\022\001\023\001\255\255\025\001\255\255\027\001\255\255\
\018\001\019\001\020\001\255\255\255\255\023\001\035\001\036\001\
\037\001\027\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\013\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\023\001\
\255\255\025\001\024\001\027\001\026\001\255\255\028\001\029\001\
\255\255\255\255\255\255\035\001\036\001\037\001"

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
  ISEMPTY\000\
  BOOL\000\
  NAT\000\
  STR\000\
  FST\000\
  SCN\000\
  LPAREN\000\
  RPAREN\000\
  LCORCH\000\
  RCORCH\000\
  LBRAC\000\
  RBRAC\000\
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
# 54 "parser.mly"
      ( VarValue _1 )
# 293 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      (VarAsignation (_1,_3))
# 301 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 60 "parser.mly"
      ( _1 )
# 308 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 62 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 317 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 64 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 326 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 66 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 335 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 68 "parser.mly"
      (TmLetIn (_2, TmFix(TmAbs(_2,_4,_6)),_8) )
# 345 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 73 "parser.mly"
      ( _1 )
# 352 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 75 "parser.mly"
      ( TmSucc _2 )
# 359 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 77 "parser.mly"
      ( TmFirst _2)
# 366 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 79 "parser.mly"
      (TmSecond _2)
# 373 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 81 "parser.mly"
      ( TmPred _2 )
# 380 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 83 "parser.mly"
      ( TmIsZero _2 )
# 387 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 85 "parser.mly"
      (TmConcat (_2,_3))
# 395 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 87 "parser.mly"
      (TmTail _2)
# 402 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 89 "parser.mly"
      (TmHead _2)
# 409 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 91 "parser.mly"
      (TmIsEmpty _2)
# 416 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 93 "parser.mly"
      ( TmApp (_1, _2) )
# 424 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 97 "parser.mly"
      ( _2 )
# 431 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 99 "parser.mly"
      ( TmPair(_2,_4) )
# 439 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
      ( TmList [] )
# 445 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lista) in
    Obj.repr(
# 103 "parser.mly"
      ( TmList (_2::_3) )
# 453 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
      ( TmRec [] )
# 459 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 107 "parser.mly"
      ( TmRec ((_2,_4)::_5) )
# 468 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
      ( TmTrue )
# 474 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
      ( TmFalse )
# 480 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
      ( TmVar _1 )
# 487 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 115 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 497 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 120 "parser.mly"
      ( TmString _1)
# 504 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'lista) in
    Obj.repr(
# 124 "parser.mly"
      (_2::_3)
# 512 "parser.ml"
               : 'lista))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
      ([])
# 518 "parser.ml"
               : 'lista))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'reg) in
    Obj.repr(
# 130 "parser.mly"
      ((_2,_4)::_5)
# 527 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "parser.mly"
      ([])
# 533 "parser.ml"
               : 'reg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 136 "parser.mly"
      ( _1 )
# 540 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 138 "parser.mly"
      ( TyArr (_1, _3) )
# 548 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 140 "parser.mly"
      ( TyPair (_1,_3))
# 556 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 142 "parser.mly"
      ( TyList _1)
# 563 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 144 "parser.mly"
      ( TyRec [] )
# 569 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'tyreg) in
    Obj.repr(
# 146 "parser.mly"
      ( TyRec ((_2,_4)::_5) )
# 578 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'tyreg) in
    Obj.repr(
# 150 "parser.mly"
      ((_2,_4)::_5)
# 587 "parser.ml"
               : 'tyreg))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
      ([])
# 593 "parser.ml"
               : 'tyreg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 156 "parser.mly"
      ( _2 )
# 600 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 158 "parser.mly"
      ( TyBool )
# 606 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
      ( TyNat )
# 612 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "parser.mly"
      ( TyStr )
# 618 "parser.ml"
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
