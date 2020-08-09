open Lexer
open Lexing
open Printf
open Syntax

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

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

let sample =
  "let not: (Bool -> Bool) = fn (b: Bool): Bool =>\n\
  \  case b of\n\
  \  | true => false\n\
  \  | false => true in\n\
   let (zero, ten): (Int, Int) = (0, 10) in\n\
   if not true then zero else (false, 10).1"

let () = parse_and_print (Lexing.from_string sample)
