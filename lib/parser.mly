%{
  (* OCaml header: type definitions, helper functions, etc. *)
  open Ast (* Assuming you have an AST module defined elsewhere *)

  (* Helper functions can be defined here *)
%}

(* Token declarations *)
%token <int> NUMBER
%token <string> STRING
%token <string> IDENTIFIER
%token TRUE FALSE
%token PLUS MINUS TIMES DIVIDE MODULO
%token EQUAL_EQUAL_EQUAL NOT_EQUAL_EQUAL
%token GREATER LESS GREATER_EQUAL LESS_EQUAL
%token NOT
%token AND OR
%token QUESTION COLON
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA SEMICOLON
%token EQUAL ARROW
%token CONST FUNCTION RETURN IF ELSE
%token EOF

(* Precedence and associativity declarations *)
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%nonassoc EQUAL_EQUAL_EQUAL NOT_EQUAL_EQUAL
%nonassoc LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT
%nonassoc ARROW

(* Starting point *)
%start <Ast.program> program

%%

program:
  | statements EOF { Program($1) }
  ;

statements:
  | /* empty */ { [] }
  | statement statements { $1 :: $2 }
  ;

statement:
  | CONST IDENTIFIER EQUAL expression SEMICOLON { ConstDeclaration($2, $4) }
  | FUNCTION IDENTIFIER LPAREN names RPAREN block { FunctionDeclaration($2, $4, $6) }
  | RETURN expression SEMICOLON { ReturnStatement($2) }
  | if_statement { $1 }
  | block { BlockStatement($1) }
  | expression SEMICOLON { ExpressionStatement($1) }
  ;

names:
  | /* empty */ { [] }
  | IDENTIFIER { [$1] }
  | IDENTIFIER COMMA names { $1 :: $3 }
  ;

if_statement:
  | IF LPAREN expression RPAREN block ELSE block { IfStatement($3, $5, $7) }
  | IF LPAREN expression RPAREN block ELSE if_statement { IfElseIfStatement($3, $5, $7) }
  ;

block:
  | LBRACE statements RBRACE { Block($2) }
  ;

expression:
  | NUMBER { NumberLiteral($1) }
  | TRUE { BooleanLiteral(true) }
  | FALSE { BooleanLiteral(false) }
  | STRING { StringLiteral($1) }
  | IDENTIFIER { NameExpression($1) }
  | expression PLUS expression { BinaryExpression(Plus, $1, $3) }
  | expression MINUS expression { BinaryExpression(Minus, $1, $3) }
  | expression TIMES expression { BinaryExpression(Times, $1, $3) }
  | expression DIVIDE expression { BinaryExpression(Divide, $1, $3) }
  | expression MODULO expression { BinaryExpression(Modulo, $1, $3) }
  | expression EQUAL_EQUAL_EQUAL expression { BinaryExpression(TripleEqual, $1, $3) }
  | expression NOT_EQUAL_EQUAL expression { BinaryExpression(NotTripleEqual, $1, $3) }
  | expression GREATER expression { BinaryExpression(Greater, $1, $3) }
  | expression LESS expression { BinaryExpression(Less, $1, $3) }
  | expression GREATER_EQUAL expression { BinaryExpression(GreaterEqual, $1, $3) }
  | expression LESS_EQUAL expression { BinaryExpression(LessEqual, $1, $3) }
  | NOT expression { UnaryExpression(Not, $2) }
  | MINUS expression %prec NOT { UnaryExpression(Negate, $2) }
  | expression AND expression { LogicalExpression(And, $1, $3) }
  | expression OR expression { LogicalExpression(Or, $1, $3) }
  | expression LPAREN expressions RPAREN { FunctionApplication($1, $3) }
  | IDENTIFIER ARROW expression { LambdaExpression([$1], ExprBody($3)) }
  | LPAREN names RPAREN ARROW expression { LambdaExpression($2, ExprBody($5)) }
  | IDENTIFIER ARROW block { LambdaExpression([$1], BlockBody($3)) }
  | LPAREN names RPAREN ARROW block { LambdaExpression($2, BlockBody($5)) }
  | expression QUESTION expression COLON expression { ConditionalExpression($1, $3, $5) }
  | LPAREN expression RPAREN { $2 }
  ;

expressions:
  | /* empty */ { [] }
  | expression { [$1] }
  | expression COMMA expressions { $1 :: $3 }
  ;
