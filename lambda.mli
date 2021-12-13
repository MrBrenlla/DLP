
type ty =
    TyBool
  | TyNat
  | TyStr
  | TyArr of ty * ty
  | TyPair of ty * ty
  | TyList of ty
  | TyEmptyList
  | TyRec of (string * ty) list
;;

type context =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmPair of term * term
  | TmFirst of term
  | TmSecond of term
  | TmList of term list
  | TmHead of term
  | TmTail of term
  | TmIsEmpty of term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmConcat of term * term
  | TmVar of string
  | TmString of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmRec of (string * term) list
;;

type variable =
      VarAsignation of string * term
    | VarValue of term
    ;;


val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> (string * term) list -> term;;
