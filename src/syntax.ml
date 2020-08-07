type id = string

type type_ = TyBool | TyInt | TyTuple of type_ list | TyFn of type_ * type_

type pattern =
  | PWildcard
  | PId of id
  | PBool of bool
  | PInt of int
  | PTuple of pattern list

type term =
  | TApply of term * term
  | TLet of pattern * type_ * term * term
  | TCase of term * (pattern * term) list
  | TIf of term * term * term
  | TId of id
  | TBool of bool
  | TInt of int
  | TFn of pattern * type_ * term
  | TTuple of term list

let output_tuple outfn = function
  | [] -> "()"
  | [ t ] -> "(" ^ outfn t ^ ",)"
  | ts -> "(" ^ String.concat ", " (List.map outfn ts) ^ ")"

let rec output_type = function
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyTuple t -> output_tuple output_type t
  | TyFn (t1, t2) -> "(" ^ output_type t1 ^ " -> " ^ output_type t2 ^ ")"

let rec output_pattern = function
  | PWildcard -> "_"
  | PId id -> id
  | PBool b -> string_of_bool b
  | PInt i -> string_of_int i
  | PTuple t -> output_tuple output_pattern t

let rec output_term = function
  | TApply (t1, t2) -> output_term t1 ^ " " ^ output_term t2
  | TLet (p, ty, t1, t2) ->
      "let " ^ output_pattern p ^ ": " ^ output_type ty ^ " = " ^ output_term t1
      ^ " in " ^ output_term t2
  | TCase (t, cases) ->
      let output_case (pat, term) =
        output_pattern pat ^ " => " ^ output_term term
      in
      "case " ^ output_term t ^ " of "
      ^ String.concat " | " (List.map output_case cases)
  | TIf (t1, t2, t3) ->
      "if " ^ output_term t1 ^ " then " ^ output_term t2 ^ " else "
      ^ output_term t3
  | TId id -> id
  | TBool b -> string_of_bool b
  | TInt i -> string_of_int i
  | TFn (p, ty, t) ->
      "fn " ^ output_pattern p ^ ": " ^ output_type ty ^ " => " ^ output_term t
  | TTuple t -> output_tuple output_term t
