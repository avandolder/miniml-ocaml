type ty =
  | TyBool
  | TyInt
  | TyTuple of ty list
  | TyFn of ty * ty

type pattern =
  | PWildcard
  | PId of string
  | PBool of bool
  | PInt of int
  | PTuple of pattern list

type term =
  | TApply of term * term
  | TLet of pattern * ty * term * term
  | TCase of term * (pattern * term) list
  | TIf of term * term * term
  | TId of string
  | TBool of bool
  | TInt of int
  | TFn of pattern * ty * term
  | TTuple of term list
