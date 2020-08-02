{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1;
    }
}

let int = '-'? ['0'-'9']+
let white = [' ' '\t' '\r']+
let newline = '\n'
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | "in" { IN }
  | "case" { CASE }
  | "of" { OF }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fn" { FN }
  | "Bool" { TYBOOL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "Int" { TYINT }
  | '_' { WILDCARD }
  | '(' { LEFT_PARAN }
  | ')' { RIGHT_PARAN }
  | ':' { COLON }
  | ',' { COMMA }
  | '|' { BAR }
  | '=' { EQUAL }
  | "=>" { FAT_ARROW }
  | "->" { THIN_ARROW }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("unexpected char: " ^ Lexing.lexeme lexbuf)) }
