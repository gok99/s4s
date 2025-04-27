(* ast_typed.ml - Modified Abstract Syntax Tree with Type Annotations *)
open Ast

(* Type definitions *)
type type_expr =
  | SimpleType of string               (* Basic types like 'number', 'string', etc. *)
  | AnswerType of type_expr * string      (* (ret, ans) format *)
  | FunctionType of type_expr list * type_expr  (* Function types with param types and return type *)
  | ReturnType of type_expr

(* Lambda body types *)
type lambda_body =
  | ExprBody of expression
  | BlockBody of block

(* Expressions *)
and expression =
  | NumberLiteral of float
  | BooleanLiteral of bool
  | StringLiteral of string
  | NameExpression of string
  | BinaryExpression of binary_operator * expression * expression
  | UnaryExpression of unary_operator * expression
  | LogicalExpression of logical_operator * expression * expression
  | FunctionApplication of expression * expression list
  | TypedLambdaExpression of (string * type_expr) list * lambda_body  (* Typed lambda expressions *)
  | ConditionalExpression of expression * expression * expression

(* Block *)
and block = Block of statement list

(* Statements *)
and statement =
  | ConstDeclaration of string * expression
  | TypedConstDeclaration of string * type_expr * expression  (* Const with type annotation *)
  | FunctionDeclaration of string * (string * type_expr) list * type_expr * block  (* Function with typed params and return type *)
  | ReturnStatement of expression
  | IfStatement of expression * block * block
  | IfElseIfStatement of expression * block * statement
  | BlockStatement of block
  | ExpressionStatement of expression

(* Program *)
type program = Program of statement list

(* Erase types *)
let program_of_typed_program (Program stmts) : Ast.program =
  let rec expression_of_typed_expression (exp: expression) : Ast.expression =
    match exp with
    | NumberLiteral n -> Ast.NumberLiteral n
    | BooleanLiteral b -> Ast.BooleanLiteral b
    | StringLiteral s -> Ast.StringLiteral s
    | NameExpression name -> Ast.NameExpression name
    | BinaryExpression (op, left, right) ->
        Ast.BinaryExpression (
          op, 
          expression_of_typed_expression left, 
          expression_of_typed_expression right
        )
    | UnaryExpression (op, expr) ->
        Ast.UnaryExpression (
          op, 
          expression_of_typed_expression expr
        )
    | LogicalExpression (op, left, right) ->
        Ast.LogicalExpression (
          op, 
          expression_of_typed_expression left, 
          expression_of_typed_expression right
        )
    | FunctionApplication (func, args) ->
        let func' = expression_of_typed_expression func in
        let args' = List.map expression_of_typed_expression args in
        Ast.FunctionApplication (func', args')
    | TypedLambdaExpression (params, body) ->
        let params' = List.map fst params in
        let body' = lambda_body_of_typed_lambda_body body in
        Ast.LambdaExpression (params', body')
    | ConditionalExpression (cond, then_exp, else_exp) ->
        Ast.ConditionalExpression (
          expression_of_typed_expression cond,
          expression_of_typed_expression then_exp,
          expression_of_typed_expression else_exp
        )
  and statement_of_typed_statement (stmt: statement) : Ast.statement =
    match stmt with
    | ConstDeclaration (name, expr) ->
        Ast.ConstDeclaration (name, expression_of_typed_expression expr)
    | TypedConstDeclaration (name, _, expr) ->
        Ast.ConstDeclaration (name, expression_of_typed_expression expr)
    | FunctionDeclaration (name, params, _, block) ->
        let params' = List.map (fun (name, _) -> name) params in
        let block' = block_of_typed_block block in
        Ast.FunctionDeclaration (name, params', block')
    | ReturnStatement expr ->
        Ast.ReturnStatement (expression_of_typed_expression expr)
    | IfStatement (cond, then_block, else_block) ->
        let cond' = expression_of_typed_expression cond in
        let then_block' = block_of_typed_block then_block in
        let else_block' = block_of_typed_block else_block in
        Ast.IfStatement (cond', then_block', else_block')
    | IfElseIfStatement (cond, then_block, else_stmt) ->
        let cond' = expression_of_typed_expression cond in
        let then_block' = block_of_typed_block then_block in
        let else_stmt' = statement_of_typed_statement else_stmt in
        Ast.IfElseIfStatement (cond', then_block', else_stmt')
    | BlockStatement block ->
        let block' = block_of_typed_block block in
        Ast.BlockStatement block'
    | ExpressionStatement expr ->
        Ast.ExpressionStatement (expression_of_typed_expression expr)
  and lambda_body_of_typed_lambda_body (body: lambda_body) : Ast.lambda_body =
    match body with
    | ExprBody expr -> Ast.ExprBody (expression_of_typed_expression expr)
    | BlockBody block -> Ast.BlockBody (block_of_typed_block block)
  and block_of_typed_block (Block stmts) : Ast.block =
    Ast.Block (List.map statement_of_typed_statement stmts)
  in
  Program (List.map statement_of_typed_statement stmts)

let rec string_of_type = function
  | SimpleType t -> t
  | AnswerType (t, ans) -> Printf.sprintf "(%s, %s)" (string_of_type t) ans
  | FunctionType (params, ret) ->
      let param_str = String.concat ", " (List.map string_of_type params) in
      Printf.sprintf "(%s) -> %s" param_str (string_of_type ret)
  | ReturnType t -> Printf.sprintf "return %s" (string_of_type t)

let string_of_unary_op op = (Machine_t.buitin_of_unary_operator op) |> Machine_t.string_of_builtin
let string_of_binary_op op = (Machine_t.buitin_of_binary_operator op) |> Machine_t.string_of_builtin
let string_of_logical_op op = (Machine_t.buitin_of_logical_operator op) |> Machine_t.string_of_builtin

let rec string_of_expression = function
  | NumberLiteral n -> Printf.sprintf "%f" n
  | BooleanLiteral b -> Printf.sprintf "%b" b
  | StringLiteral s -> Printf.sprintf "\"%s\"" s
  | NameExpression name -> name
  | BinaryExpression (op, left, right) ->
      Printf.sprintf "(%s %s %s)" (string_of_expression left) (string_of_binary_op op) (string_of_expression right)
  | UnaryExpression (op, expr) ->
      Printf.sprintf "(%s %s)" (string_of_unary_op op) (string_of_expression expr)
  | LogicalExpression (op, left, right) ->
      Printf.sprintf "(%s %s %s)" (string_of_expression left) (string_of_logical_op op) (string_of_expression right)
  | FunctionApplication (func, args) ->
      let args_str = String.concat ", " (List.map string_of_expression args) in
      Printf.sprintf "%s(%s)" (string_of_expression func) args_str
  | TypedLambdaExpression (params, _) ->
      let params_str = String.concat ", " (List.map fst params) in
      Printf.sprintf "lambda (%s) => body" params_str
  | ConditionalExpression (cond, then_exp, else_exp) ->
      Printf.sprintf "(if %s then %s else %s)" 
        (string_of_expression cond) 
        (string_of_expression then_exp) 
        (string_of_expression else_exp)
