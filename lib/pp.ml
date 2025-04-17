(* pretty_printer.ml - Pretty printer for Source §1 AST *)

open Ast
open Format

(* Helper function to indent text *)
let indent_string n s =
  let indent = String.make n ' ' in
  String.split_on_char '\n' s
  |> List.map (fun line -> if line = "" then line else indent ^ line)
  |> String.concat "\n"

(* Pretty print binary operators *)
let string_of_binary_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | TripleEqual -> "==="
  | NotTripleEqual -> "!=="
  | Greater -> ">"
  | Less -> "<"
  | GreaterEqual -> ">="
  | LessEqual -> "<="

(* Pretty print unary operators *)
let string_of_unary_op = function
  | Not -> "!"
  | Negate -> "-"

(* Pretty print logical operators *)
let string_of_logical_op = function
  | And -> "&&"
  | Or -> "||"

(* Pretty print expressions *)
let rec string_of_expression = function
  | NumberLiteral n -> string_of_float n
  | BooleanLiteral b -> string_of_bool b
  | StringLiteral s -> s
  | NameExpression name -> name
  | BinaryExpression (op, e1, e2) ->
      sprintf "(%s %s %s)"
        (string_of_expression e1)
        (string_of_binary_op op)
        (string_of_expression e2)
  | UnaryExpression (op, e) ->
      sprintf "%s(%s)"
        (string_of_unary_op op)
        (string_of_expression e)
  | LogicalExpression (op, e1, e2) ->
      sprintf "(%s %s %s)"
        (string_of_expression e1)
        (string_of_logical_op op)
        (string_of_expression e2)
  | FunctionApplication (func, args) ->
      sprintf "%s(%s)"
        (string_of_expression func)
        (String.concat ", " (List.map string_of_expression args))
  | LambdaExpression (params, body) ->
      let params_str = match params with
        | [p] -> p
        | _ -> sprintf "(%s)" (String.concat ", " params)
      in
      let body_str = match body with
        | ExprBody e -> string_of_expression e
        | BlockBody b -> string_of_block b
      in
      sprintf "%s => %s" params_str body_str
  | ConditionalExpression (cond, then_expr, else_expr) ->
      sprintf "(%s ? %s : %s)"
        (string_of_expression cond)
        (string_of_expression then_expr)
        (string_of_expression else_expr)

(* Pretty print blocks *)
and string_of_block (Block stmts) =
  let stmts_str = String.concat "\n" (List.map string_of_statement stmts) in
  sprintf "{\n%s\n}" (indent_string 2 stmts_str)

(* Pretty print statements *)
and string_of_statement = function
  | ConstDeclaration (name, expr) ->
      sprintf "const %s = %s;" name (string_of_expression expr)
  | FunctionDeclaration (name, params, body) ->
      sprintf "function %s(%s) %s"
        name
        (String.concat ", " params)
        (string_of_block body)
  | ReturnStatement expr ->
      sprintf "return %s;" (string_of_expression expr)
  | IfStatement (cond, then_block, else_block) ->
      sprintf "if (%s) %s\nelse %s"
        (string_of_expression cond)
        (string_of_block then_block)
        (string_of_block else_block)
  | IfElseIfStatement (cond, then_block, else_if) ->
      sprintf "if (%s) %s\nelse %s"
        (string_of_expression cond)
        (string_of_block then_block)
        (string_of_statement else_if)
  | BlockStatement block ->
      string_of_block block
  | ExpressionStatement expr ->
      sprintf "%s;" (string_of_expression expr)
  | WhileStatement (cond, body) ->
      sprintf "while (%s) %s"
        (string_of_expression cond)
        (string_of_block body)
  | BreakStatement ->
      "break;"
  | ContinueStatement ->
      "continue;"
  | LetDeclaration (name, expr) ->
      sprintf "let %s = %s;" name (string_of_expression expr)
  | AssignmentStatement (name, expr) ->
      sprintf "%s = %s;" name (string_of_expression expr)

(* Pretty print program *)
let string_of_program (Program stmts) =
  String.concat "\n" (List.map string_of_statement stmts)

(* Function to pretty-print to a formatter *)
let pp_program fmt program =
  pp_print_string fmt (string_of_program program)

(* Function to pretty-print to stdout *)
let print_program program =
  print_endline (string_of_program program)

(* Function to pretty-print to a string *)
let pretty_print_program program =
  string_of_program program
