%{
  (* OCaml header: type definitions, helper functions, etc. *)
  open Ast_typed (* Assuming you have an AST module defined elsewhere *)

  (* Helper functions can be defined here *)
%}

(* Token declarations *)
%token <float> NUMBER
%token <string> STRING
%token <string> IDENTIFIER
%token <string> TYPE_IDENTIFIER
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
%token COLON_COLON  /* :: for type annotations */
%token TYPE_ARROW   /* => for return types */
%token EOF

(* Precedence and associativity declarations *)
%right ARROW           /* Lowest precedence, right associative */
%nonassoc QUESTION
%nonassoc COLON
%left OR
%left AND
%nonassoc EQUAL_EQUAL_EQUAL NOT_EQUAL_EQUAL
%nonassoc LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%right NOT             /* Highest precedence */

(* Starting point *)
%start <Ast_typed.program> program

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
  | CONST IDENTIFIER COLON_COLON type_annotation EQUAL expression SEMICOLON { TypedConstDeclaration($2, $4, $6) }
  | FUNCTION IDENTIFIER LPAREN typed_names RPAREN COLON_COLON type_annotation block { FunctionDeclaration($2, $4, $7, $8) }
  | RETURN expression SEMICOLON { ReturnStatement($2) }
  | if_statement { $1 }
  | block { BlockStatement($1) }
  | expression SEMICOLON { ExpressionStatement($1) }
  ;

typed_names:
  | /* empty */ { [] }
  | typed_name { [$1] }
  | typed_name COMMA typed_names { $1 :: $3 }
  ;

typed_name:
  | IDENTIFIER COLON_COLON type_annotation { ($1, $3) }
  ;

type_annotation:
  | TYPE_IDENTIFIER { SimpleType($1) }
  | LPAREN type_annotation COMMA TYPE_IDENTIFIER RPAREN { AnswerType($2, $4) }
  | LPAREN type_annotation_list RPAREN TYPE_ARROW type_annotation { FunctionType($2, $5) }
  ;

type_annotation_list:
  | /* empty */ { [] }
  | type_annotation { [$1] }
  | type_annotation COMMA type_annotation_list { $1 :: $3 }
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
  | IDENTIFIER COLON_COLON type_annotation ARROW expression { TypedLambdaExpression([($1, $3)], ExprBody($5)) }
  | LPAREN typed_names RPAREN ARROW expression { TypedLambdaExpression($2, ExprBody($5)) }
  | IDENTIFIER COLON_COLON type_annotation ARROW block { TypedLambdaExpression([($1, $3)], BlockBody($5)) }
  | LPAREN typed_names RPAREN ARROW block { TypedLambdaExpression($2, BlockBody($5)) }
  | expression QUESTION expression COLON expression { ConditionalExpression($1, $3, $5) }
  | LPAREN expression RPAREN { $2 }
  ;

expressions:
  | /* empty */ { [] }
  | expression { [$1] }
  | expression COMMA expressions { $1 :: $3 }
  ;
