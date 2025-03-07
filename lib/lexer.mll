{
open Parser  (* The type token is defined in parser.mli *)
open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum + 1
    }
}

let digit = ['0'-'9']
let int = '-'? digit+
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let float = int frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id_start = ['_' '$' 'a'-'z' 'A'-'Z']
let id_cont = ['_' '$' 'a'-'z' 'A'-'Z' '0'-'9']
let identifier = id_start id_cont*

let string1 = '"' ([^ '"' '\n'])* '"'
let string2 = '\'' ([^ '\'' '\n'])* '\''
let string3 = '`' ([^ '`'])* '`'

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*"     { comment lexbuf; read lexbuf }
  | "//"     { line_comment lexbuf; read lexbuf }
  | int      { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { NUMBER (int_of_string (Lexing.lexeme lexbuf)) } (* Simplified for this example *)
  | "true"   { TRUE }
  | "false"  { FALSE }
  | string1  { STRING (Lexing.lexeme lexbuf) }
  | string2  { STRING (Lexing.lexeme lexbuf) }
  | string3  { STRING (Lexing.lexeme lexbuf) }
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { TIMES }
  | "/"      { DIVIDE }
  | "%"      { MODULO }
  | "==="    { EQUAL_EQUAL_EQUAL }
  | "!=="    { NOT_EQUAL_EQUAL }
  | ">"      { GREATER }
  | "<"      { LESS }
  | ">="     { GREATER_EQUAL }
  | "<="     { LESS_EQUAL }
  | "!"      { NOT }
  | "&&"     { AND }
  | "||"     { OR }
  | "?"      { QUESTION }
  | ":"      { COLON }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "{"      { LBRACE }
  | "}"      { RBRACE }
  | ","      { COMMA }
  | ";"      { SEMICOLON }
  | "="      { EQUAL }
  | "=>"     { ARROW }
  | "const"  { CONST }
  | "function" { FUNCTION }
  | "return" { RETURN }
  | "if"     { IF }
  | "else"   { ELSE }
  | identifier as id { IDENTIFIER id }
  | eof      { EOF }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | "*/"     { () }
  | newline  { next_line lexbuf; comment lexbuf }
  | _        { comment lexbuf }

and line_comment = parse
  | newline  { next_line lexbuf }
  | eof      { () }
  | _        { line_comment lexbuf }
