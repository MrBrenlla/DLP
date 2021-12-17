
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
%token ISEMPTY
%token PROJECT
%token BOOL
%token NAT
%token STR
%token FST
%token SCN
%token LPAREN
%token RPAREN
%token LCORCH
%token RCORCH
%token LBRAC
%token RBRAC
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
      { TmConcat ($2,$3)}
  | TAIL atomicTerm
      { TmTail $2}
  | HEAD atomicTerm
      { TmHead $2}
  | ISEMPTY atomicTerm
      { TmIsEmpty $2}
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | PROJECT STRINGV atomicTerm
      { TmProject ($2,$3) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | LPAREN term COMA term RPAREN
      { TmPair($2,$4) }
  | LCORCH RCORCH COLON ty
      { TmEmptyList $4 }
  | LCORCH term lista
      { TmList ($2,$3) }
  | term COLON COLON term
      { TmList ($1,$4) }
  | LBRAC RBRAC
      { TmRec [] }
  | LBRAC STRINGV EQ term reg
      { TmRec (($2,$4)::$5) }
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
      {TmList ($2,$3)}
  | RCORCH COLON ty
      {TmEmptyList $3}

reg :
    COMA STRINGV EQ term reg
      {($2,$4)::$5}
  | RBRAC
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
  | LBRAC RBRAC
      { TyRec [] }
  | LBRAC STRINGV COLON ty tyreg
      { TyRec (($2,$4)::$5) }

tyreg :
    COMA STRINGV COLON ty tyreg
      {($2,$4)::$5}
  | RBRAC
      {[]}

atomicTy :
    LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STR
      { TyStr }
