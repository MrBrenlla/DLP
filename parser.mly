
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token CONCAT
%token LET
%token LETREC
%token IN
%token LIST
%token HEAD
%token TAIL
%token BOOL
%token NAT
%token STR
%token FST
%token SCN
%token LPAREN
%token RPAREN
%token LCORCH
%token RCORCH
%token COMA
%token DOT
%token EQ
%token AST
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> STRINGV
%token <string> STRING


%start s
%type <Lambda.variable> s

%%

s :
    term EOF
      { VarValue $1 }
    | STRINGV EQ term EOF
      {VarAsignation ($1,$3)}

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      {TmLetIn ($2, TmFix(TmAbs($2,$4,$6)),$8) }


appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | FST atomicTerm
      { TmFirst $2}
  | SCN atomicTerm
      {TmSecond $2}
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      {TmConcat ($2,$3)}
  | TAIL atomicTerm
      {TmTail $2}
  | HEAD atomicTerm
      {TmHead $2}
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LPAREN term COMA term RPAREN
    { TmPair($2,$4)}
  | LCORCH RCORCH
      {TmList []}
  | LCORCH term lista
    {TmList ($2::$3)}
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRING
      { TmString $1}

lista :
    COMA term lista
      {$2::$3}
  | RCORCH
      {[]}

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }
  | ty AST ty
      { TyPair ($1,$3)}
  | ty LIST
      { TyList $1}

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STR
      { TyStr }
