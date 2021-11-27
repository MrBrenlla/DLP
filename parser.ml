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
  | LPAREN
  | RPAREN
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
# 34 "parser.ml"
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
  273 (* LPAREN *);
  274 (* RPAREN *);
  275 (* DOT *);
  276 (* EQ *);
  277 (* COLON *);
  278 (* ARROW *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  279 (* INTV *);
  280 (* STRINGV *);
  281 (* STRING *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\003\000\005\000\005\000\005\000\
\005\000\005\000\005\000\004\000\004\000\006\000\006\000\006\000\
\006\000\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\006\000\006\000\006\000\008\000\001\000\
\002\000\002\000\002\000\003\000\002\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\016\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\000\000\019\000\
\026\000\000\000\000\000\008\000\000\000\017\000\000\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\001\000\
\013\000\000\000\000\000\012\000\000\000\000\000\014\000\000\000\
\023\000\024\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\005\000\021\000\004\000\006\000\000\000\000\000\007\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\045\000\020\000\046\000"

let yysindex = "\007\000\
\009\255\000\000\246\254\000\000\000\000\051\255\020\255\020\255\
\020\255\020\255\003\255\004\255\051\255\000\000\010\255\000\000\
\000\000\035\000\020\255\000\000\015\255\000\000\033\255\000\000\
\000\000\000\000\020\255\022\255\026\255\038\255\051\255\000\000\
\000\000\245\254\051\255\000\000\051\255\245\254\000\000\051\000\
\000\000\000\000\000\000\245\254\045\255\043\255\060\255\054\255\
\053\255\000\000\059\255\051\255\245\254\051\255\051\255\051\255\
\000\000\000\000\000\000\000\000\000\000\065\255\051\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\021\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\000\000\227\255\062\000\000\000"

let yytablesize = 282
let yytable = "\023\000\
\017\000\003\000\041\000\042\000\043\000\044\000\030\000\001\000\
\049\000\003\000\004\000\005\000\006\000\021\000\051\000\007\000\
\008\000\009\000\010\000\011\000\012\000\004\000\005\000\059\000\
\040\000\013\000\028\000\029\000\047\000\031\000\048\000\014\000\
\015\000\016\000\032\000\034\000\013\000\035\000\020\000\020\000\
\020\000\037\000\014\000\022\000\016\000\058\000\038\000\060\000\
\061\000\062\000\050\000\003\000\004\000\005\000\006\000\039\000\
\064\000\007\000\008\000\009\000\010\000\011\000\012\000\052\000\
\053\000\054\000\055\000\013\000\024\000\025\000\026\000\027\000\
\056\000\014\000\022\000\016\000\057\000\063\000\000\000\000\000\
\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\017\000\000\000\000\000\003\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\017\000\000\000\003\000\000\000\000\000\000\000\017\000\
\017\000\017\000"

let yycheck = "\006\000\
\000\000\000\000\014\001\015\001\016\001\017\001\013\000\001\000\
\038\000\001\001\002\001\003\001\004\001\024\001\044\000\007\001\
\008\001\009\001\010\001\011\001\012\001\002\001\003\001\053\000\
\031\000\017\001\024\001\024\001\035\000\020\001\037\000\023\001\
\024\001\025\001\000\000\021\001\017\001\005\001\018\001\019\001\
\020\001\020\001\023\001\024\001\025\001\052\000\021\001\054\000\
\055\000\056\000\000\000\001\001\002\001\003\001\004\001\018\001\
\063\000\007\001\008\001\009\001\010\001\011\001\012\001\019\001\
\022\001\006\001\013\001\017\001\007\000\008\000\009\000\010\000\
\020\001\023\001\024\001\025\001\018\001\013\001\255\255\255\255\
\019\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\027\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\255\255\005\001\006\001\
\255\255\255\255\255\255\255\255\255\255\255\255\013\001\255\255\
\255\255\017\001\255\255\018\001\255\255\255\255\255\255\023\001\
\024\001\025\001"

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
  LPAREN\000\
  RPAREN\000\
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
# 42 "parser.mly"
      ( VarValue _1 )
# 231 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 44 "parser.mly"
      (VarAsignation (_1,_3))
# 239 "parser.ml"
               : Lambda.variable))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 48 "parser.mly"
      ( _1 )
# 246 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 50 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 255 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 52 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 264 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 54 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 273 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      (TmLetIn (_2, TmFix(TmAbs(_2,_4,_6)),_8) )
# 283 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 61 "parser.mly"
      ( _1 )
# 290 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 63 "parser.mly"
      ( TmSucc _2 )
# 297 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 65 "parser.mly"
      ( TmPred _2 )
# 304 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 67 "parser.mly"
      ( TmIsZero _2 )
# 311 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'atomicTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 69 "parser.mly"
      (TmConcat (_2,_3))
# 319 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 71 "parser.mly"
      ( TmApp (_1, _2) )
# 327 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 75 "parser.mly"
      ( _2 )
# 334 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
      ( TmTrue )
# 340 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
      ( TmFalse )
# 346 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
      ( TmVar _1 )
# 353 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 83 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 363 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
      ( TmString _1)
# 370 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 92 "parser.mly"
      ( _1 )
# 377 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 94 "parser.mly"
      ( TyArr (_1, _3) )
# 385 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 98 "parser.mly"
      ( _2 )
# 392 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser.mly"
      ( TyBool )
# 398 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
      ( TyNat )
# 404 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
      ( TyStr )
# 410 "parser.ml"
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
