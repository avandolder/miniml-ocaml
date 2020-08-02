open Lexer
open Lexing
open Printf

open Syntax

let output_tuple outfn = function
  | [] -> "()"
  | [t] -> "(" ^ outfn t ^ ",)"
  | ts -> "(" ^ String.concat ", " (List.map outfn ts) ^ ")"

let rec output_ty = function
  | TyBool -> "Bool"
  | TyInt -> "Int"
  | TyTuple t -> output_tuple output_ty t
  | TyFn (t1, t2) -> "(" ^ output_ty t1 ^ " -> " ^ output_ty t2 ^ ")"

let rec output_pattern = function
  | PWildcard -> "_"
  | PId id -> id
  | PBool b -> string_of_bool b
  | PInt i -> string_of_int i
  | PTuple t -> output_tuple output_pattern t

let rec output_term = function
  | TApply (t1, t2) -> output_term t1 ^ " " ^ output_term t2
  | TLet (p, ty, t1, t2) ->
    "let " ^ output_pattern p ^ ": " ^ output_ty ty ^ " = " ^ output_term t1 ^ " in " ^ output_term t2
  | TCase (t, cases) ->
    let output_case (pat, term) = output_pattern pat ^ " => " ^ output_term term in
    "case " ^ output_term t ^ " of " ^ String.concat " | " (List.map output_case cases)
  | TIf (t1, t2, t3) ->
    "if " ^ output_term t1 ^ " then " ^ output_term t2 ^ " else " ^ output_term t3
  | TId id -> id
  | TBool b -> string_of_bool b
  | TInt i -> string_of_int i
  | TFn (p, ty, t) ->
    "fn " ^ output_pattern p ^ ": " ^ output_ty ty ^ " => " ^ output_term t
  | TTuple t -> output_tuple output_term t

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some t ->
    printf "%s\n" (output_term t);
    parse_and_print lexbuf
  | None -> ()

let sample = "let not: (Bool -> Bool) = fn b: Bool =>
  case b of
  | true => false
  | false => true in
let (zero, ten): (Int, Int) = (0, 10) in
if not true then zero else 10"

let () =
    parse_and_print (Lexing.from_string sample)
