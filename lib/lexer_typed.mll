{
(* We need to handle both parsers, so we use a module alias *)
module P = Parser_typed
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

(* Type identifiers typically start with uppercase letters *)
let type_id_start = ['A'-'Z']
let type_identifier = type_id_start id_cont*

let string1 = '"' ([^ '"' '\n'])* '"'
let string2 = '\'' ([^ '\'' '\n'])* '\''
let string3 = '`' ([^ '`'])* '`'

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*"     { comment lexbuf; read lexbuf }
  | "//"     { line_comment lexbuf; read lexbuf }
  | float    { P.NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"   { P.TRUE }
  | "false"  { P.FALSE }
  | string1  { P.STRING (Lexing.lexeme lexbuf) }
  | string2  { P.STRING (Lexing.lexeme lexbuf) }
  | string3  { P.STRING (Lexing.lexeme lexbuf) }
  | "+"      { P.PLUS }
  | "-"      { P.MINUS }
  | "*"      { P.TIMES }
  | "/"      { P.DIVIDE }
  | "%"      { P.MODULO }
  | "==="    { P.EQUAL_EQUAL_EQUAL }
  | "!=="    { P.NOT_EQUAL_EQUAL }
  | ">"      { P.GREATER }
  | "<"      { P.LESS }
  | ">="     { P.GREATER_EQUAL }
  | "<="     { P.LESS_EQUAL }
  | "!"      { P.NOT }
  | "&&"     { P.AND }
  | "||"     { P.OR }
  | "?"      { P.QUESTION }
  | ":"      { P.COLON }
  | "::"     { P.COLON_COLON }  (* New token for type annotations *)
  | "->"     { P.TYPE_ARROW }   (* Reusing ARROW as TYPE_ARROW *)
  | "=>"     { P.ARROW }        (* Adding arrow for lambdas to differentiate from type arrow *)
  | "("      { P.LPAREN }
  | ")"      { P.RPAREN }
  | "{"      { P.LBRACE }
  | "}"      { P.RBRACE }
  | ","      { P.COMMA }
  | ";"      { P.SEMICOLON }
  | "="      { P.EQUAL }
  | "const"  { P.CONST }
  | "function" { P.FUNCTION }
  | "return" { P.RETURN }
  | "if"     { P.IF }
  | "else"   { P.ELSE }
  | type_identifier as tid { P.TYPE_IDENTIFIER tid }  (* New token for type names *)
  | identifier as id { P.IDENTIFIER id }
  | eof      { P.EOF }
  | _        { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
  | "*/"     { () }
  | newline  { next_line lexbuf; comment lexbuf }
  | _        { comment lexbuf }

and line_comment = parse
  | newline  { next_line lexbuf }
  | eof      { () }
  | _        { line_comment lexbuf }
