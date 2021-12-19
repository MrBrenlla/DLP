
exception Type_error of string
;;
exception Failure of string
;;

(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyStr
  | TyArr of ty * ty
  | TyPair of ty * ty
  | TyList of ty
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
  | TmList of term * term
  | TmEmptyList of ty
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
  | TmProject of string * term
;;

type variable =
      VarAsignation of string * term
    | VarValue of term
    ;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addbinding ctx x bind =
  (x, bind) :: ctx
;;

let getbinding ctx x =
  List.assoc x ctx
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyStr->
      "Str"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyPair (ty1,ty2)->
      string_of_ty ty1^"*"^ string_of_ty ty2
  | TyList ty1 ->
       string_of_ty ty1^" list"
  | TyRec l ->
      let aux (a,b) = a^":"^(string_of_ty b) in
      let ty' = String.concat "," (List.map aux l) in
      "{"^ty'^"}"
;;


let rec rec_subtype l = function
    (name,ty)::t ->  (try
              let ty' = List.assoc name l in
              if (ty=ty') then rec_subtype l t
              else false
              with
              Not_found -> false)
    |[]-> true



let rec subtype t1 t2 =
    match t1,t2 with
    (TyRec l1, TyRec l2) -> rec_subtype l1 l2
    |TyArr(t11,t12),TyArr(t21,t22)-> (subtype t11 t21) && (subtype t22 t12)
    |_-> t1=t2


let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        let tyT3 = typeof ctx t3 in
        if (subtype tyT2 tyT3) then tyT3
        else if (subtype tyT3 tyT2) then tyT2
            else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

  | TmPair(t1,t2) ->
      TyPair(typeof ctx t1,typeof ctx t2)

  | TmFirst (t)->
      let t'= typeof ctx t in
      (match t' with
        TyPair(t1,_)->t1
        | _ -> raise (Type_error "argument of First is not a pair"))

  | TmSecond(t)->
      let t'= typeof ctx t in
      (match t' with
          TyPair(_,t2)->t2
          | _ -> raise (Type_error "argument of First is not a pair"))

  | TmEmptyList ty->
      TyList ty

  | TmList(tm1,tm2) ->
      let ty1' =   typeof ctx tm1
      in
      let ty2' =  typeof ctx tm2
      in
      if (TyList ty1' = ty2') then ty2'
      else raise (Type_error (string_of_ty ty2'^" can't have a "^string_of_ty ty1'))


  |TmTail t ->
      let t'= typeof ctx t in
      (match t' with
            TyList ty -> TyList ty
          | _ -> raise (Type_error "argument of tail is not a list"))

  | TmHead t ->
      let t'= typeof ctx t in
        (match t' with
              TyList ty->ty
            | _ -> raise (Type_error "argument of head is not a list"))
  | TmIsEmpty t ->
    let t'= typeof ctx t in
      (match t' with
            TyList _->TyBool
          | _ -> raise (Type_error "argument of isempty is not a list"))

  | TmString s->
      TyStr

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

  | TmConcat (t1,t2) ->
      if ((typeof ctx t1 = TyStr) && (typeof ctx t2 = TyStr)) then TyStr
      else raise (Type_error "arguments of concat are not strings")



    (* T-Var *)
  | TmVar x ->
      (try getbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if (subtype tyT2 tyT11) then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2

  | TmFix t1 ->
    let tyT1 = typeof ctx t1 in
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if (subtype tyT12 tyT11) then tyT12
          else raise (Type_error "result of body not compatible with domain")
       | _ -> raise (Type_error "arrow type expected"))

  | TmRec l ->
      let aux (a,b) = (a,typeof ctx b) in
      TyRec ( List.map aux l )
  | TmProject (name,t) ->
      let ty'= typeof ctx t in
      (match ty' with
        TyRec l ->(try
                    List.assoc name l
                  with
                    Not_found->raise (Type_error "projection not valid for this record"))
        |_ -> raise (Type_error "second argument of projection is not a record"))
;;




(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t

  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"

  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"

  | TmPair(t1,t2)->
        "("^ string_of_term t1^","^ string_of_term t2 ^")"

  | TmConcat (t1,t2) ->
      "concat"^"(" ^ string_of_term t1 ^ ") ("^string_of_term t2 ^ ")"

  | TmFirst(t1)->
      "first" ^ "(" ^ string_of_term t1 ^ ")"

  | TmSecond (t1) ->
      "second" ^ "(" ^ (string_of_term t1) ^ ")"

  | TmEmptyList ty->
      "[]:"^string_of_ty ty

  | TmList (h,TmList (a,b)) ->
      let rec string_of_list = function
            TmList(h,t) -> ","^string_of_term h ^ string_of_list t
          | TmEmptyList ty-> "]:"^string_of_ty ty
          | t -> raise (Failure ("Malformed list construnction using "^string_of_term(t)))
      in
      "["^ string_of_term h^ string_of_list (TmList (a,b))
  | TmList (h,TmEmptyList ty) ->
      "["^string_of_term h^"]:"^string_of_ty ty
  | TmList (h,t) ->
      "("^string_of_term h^"::"^string_of_term t^")"
  | TmHead(t1) ->
      "head" ^ "(" ^ (string_of_term t1) ^ ")"

  | TmTail(t1) ->
      "tail" ^ "(" ^ (string_of_term t1) ^ ")"

  | TmIsEmpty(t1) ->
      "isempty" ^ "(" ^ (string_of_term t1) ^ ")"

  | TmString s ->
      "\""^s^"\""
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t->
      "(fix"^ string_of_term t ^ ")"
  | TmRec l ->
      let aux (a,b) = a^"="^(string_of_term b) in
      let tm' = String.concat "," (List.map aux l) in
      "{"^tm'^"}"
  | TmProject (name,t) ->
      "(project "^name^" "^string_of_term t^")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmString t->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmPair(t1,t2)->
      lunion (free_vars t1) (free_vars t2)
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmFirst t->
      free_vars t
  | TmSecond t->
      free_vars t
  | TmEmptyList _ ->
      []
  | TmList (t1,t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmHead t->
      free_vars t
  | TmTail t->
      free_vars t
  | TmIsEmpty t->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmRec l ->
      let aux(a,b)= free_vars b in
      List.fold_left lunion [] (List.map aux l)
  | TmProject (_,t)->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmString s->
      TmString s
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmPair (t1, t2) ->
      TmPair (subst x s t1, subst x s t2)
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmFirst t ->
      TmFirst (subst x s t)
  | TmSecond t ->
      TmSecond (subst x s t)
  |TmEmptyList t->
      TmEmptyList t
  | TmList (t1,t2) ->
      TmList( (subst x s t1) ,(subst x s t2))
  | TmHead t ->
      TmHead (subst x s t)
  | TmTail t ->
      TmTail (subst x s t)
  | TmIsEmpty t ->
      TmIsEmpty (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmRec l ->
      let aux(a,b)=(a,subst x s b) in
      TmRec (List.map aux l)
  | TmProject (name,t)->
      TmProject(name,subst x s t)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmPair (_,_) -> true
  | TmList _ -> true
  | TmEmptyList _ -> true
  | TmRec _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;



let rec eval1 tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1
    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 t1 in
      TmIsZero t1'

  |TmPair(t1,t2) when isval t1->
      TmPair(eval1 t1, eval1 t2)

  |TmPair(t1,t2)->
      TmPair(eval1 t1, t2)

  |TmConcat (TmString t1, TmString t2)->
      TmString(t1^t2)

  |TmConcat (TmString t1,t2)->
      TmConcat(TmString t1, eval1 t2)

  |TmConcat (t1, TmString t2)->
      TmConcat(eval1 t1, TmString t2)

  |TmConcat (t1,t2)->
      TmConcat(eval1 t1, eval1 t2)

  |TmFirst(TmPair(t1,_)) ->
      t1

  |TmFirst t ->
      TmFirst (eval1 t)

  |TmSecond(TmPair(_,t2)) ->
      t2

  |TmSecond t ->
      TmSecond(eval1 t)

  |TmList (t1,t2) ->
      if (isval t1) then TmList(t1, eval1 t2)
      else TmList(eval1 t1, t2)

  |TmHead(TmList(h,_)) when isval h ->
      h

  |TmHead(TmEmptyList _)->
      raise (Failure "EmptyList has no head")

  |TmHead t ->
      TmHead (eval1 t)

  |TmTail(TmList(_,t)) when isval t ->
        t

  |TmTail(TmEmptyList _)->
      raise (Failure "EmptyList has no tail")

  |TmTail t ->
        TmTail (eval1 t)

  |TmIsEmpty t -> (match t with
        TmList _ -> TmFalse
        |TmEmptyList _ -> TmTrue
        | _-> TmIsEmpty (eval1 t))

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 t1 in
      TmLetIn (x, t1', t2)

    (*E-FixBeta*)
  | TmFix (TmAbs(x,_,t12))->
      subst x tm t12

    (*E-Fix*)
  |TmFix t1->
      let t1'= eval1 t1 in
      TmFix t1'

  |TmRec (l) ->
      let rec aux= function
        (a,b)::t -> if (isval b) then (a,b)::(aux t)
                    else (a,eval1 b)::t
        |[]-> raise NoRuleApplies
      in
      TmRec (aux l)

  |TmProject (name,TmRec(l))->
      (try
      List.assoc name l
      with
      Not_found-> raise (Failure "This register doesn't have that value"))

  |TmProject (name,t)->
      TmProject(name,eval1 t)

  | _ ->
      raise NoRuleApplies
;;

let rec use_vars tm = function
    (x,s)::t-> use_vars (subst x s tm) t
  | []-> tm
;;

let rec eval_loop tm =
  try
    let tm' = eval1 tm in
    eval_loop tm'
  with
    NoRuleApplies -> tm
;;

let eval tm vars=
  let tm'=eval_loop tm in
  eval_loop (use_vars tm' vars)
;;
