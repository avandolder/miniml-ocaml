open Lexer
open Lexing
open Printf

open Syntax

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
    printf "%s\n" (output_type (Typecheck.check_term [] t));
    parse_and_print lexbuf
  | None -> ()

let sample = "let not: (Bool -> Bool) = fn b: Bool -> Bool =>
  case b of
  | true => false
  | false => true in
let (zero, ten): (Int, Int) = (0, 10) in
if not true then zero else 10"

let () =
    parse_and_print (Lexing.from_string sample)
