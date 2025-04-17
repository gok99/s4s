(* ast.ml - Abstract Syntax Tree for Source §4 + Delimited Control *)

(* Binary operators *)
type binary_operator =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | TripleEqual
  | NotTripleEqual
  | Greater
  | Less
  | GreaterEqual
  | LessEqual

(* Unary operators *)
type unary_operator =
  | Not
  | Negate

(* Logical operators *)
type logical_operator =
  | And
  | Or

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
  | LambdaExpression of string list * lambda_body
  | ConditionalExpression of expression * expression * expression

(* Block *)
and block = Block of statement list

(* Statements *)
and statement =
  | ConstDeclaration of string * expression
  | LetDeclaration of string * expression
  | AssignmentStatement of string * expression
  | FunctionDeclaration of string * string list * block
  | ReturnStatement of expression
  | IfStatement of expression * block * block
  | IfElseIfStatement of expression * block * statement
  | WhileStatement of expression * block
  | BreakStatement
  | ContinueStatement
  | BlockStatement of block
  | ExpressionStatement of expression

(* Program *)
type program = Program of statement list
