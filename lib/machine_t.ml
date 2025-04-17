open Ast
open Pp

type value =
  | Number of float
  | String of string
  | Boolean of bool
  | Undefined
  | Null
  | UnassignedValue
  | Pair of { 
      fst: value; 
      snd: value 
    }
  | Closure of {
      params: string list;
      body: lambda_body;
      env: environment
    }
  | Builtin of builtin
  | Continuation of {
      stash: value list;
      control: command list;
      env: environment;
      push_handler: handler option
    }
  | Handler of handler
  (* 
    Not a real value. Our discipline with handling
    markers will ensure that this is never accessible 
    to the program.
  *)
  | ResetStashMarker

and builtin =
  | Display
  | Stringify
  | Math_Sqrt
  | Math_Round
  | Is_Number
  | Is_String
  | Is_Function
  | Is_Boolean
  | Is_Undefined
  | Pair
  | Head
  | Tail
  | Is_Null
  | Is_Pair
  | List
  | Shift
  | Reset
  | Perform
  (*  *)
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
  | Not
  | Negate
  | And
  | Or

and command =
  | Expression of expression
  | Statement of statement

  (* Instructions - internal operations *)
  | PopInstr
  | LitInstr of value
  | UnopInstr of builtin
  | BinopInstr of builtin
  (* | VaropInstr of builtin * int *)
  | AppInstr of int
  | AssignmentInstr of string
  | BranchInstr of { cons: statement; alt: statement }
  | WhileInstr of { pred: expression; body: statement }
  | EnvInstr of environment
  | ResetInstr
  | MarkInstr
  | ResetControlMarker
  | PopHandlerInstr
  | RunWithHandlerInstr of block
  | PerformInstr of string * int

and handler = (string * value) list

and control = command list
and stash = value list
and environment = (((string, value) Hashtbl.t) ref) list
and handler_stack = handler list

let string_of_builtin = function
  | Display -> "display"
  | Stringify -> "stringify"
  | Math_Sqrt -> "math_sqrt"
  | Math_Round -> "math_round"
  | Is_Number -> "is_number"
  | Is_String -> "is_string"
  | Is_Function -> "is_function"
  | Is_Boolean -> "is_boolean"
  | Is_Undefined -> "is_undefined"
  | Pair -> "pair"
  | Head -> "head"
  | Tail -> "tail"
  | Is_Null -> "is_null"
  | Is_Pair -> "is_pair"
  | List -> "list"
  | Shift -> "shift"
  | Reset -> "reset"
  | Perform -> "perform"
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
  | Not -> "!"
  | Negate -> "-"
  | And -> "&&"
  | Or -> "||"

let buitin_of_binary_operator: binary_operator -> builtin = function
  | Plus -> Plus
  | Minus -> Minus
  | Times -> Times
  | Divide -> Divide
  | Modulo -> Modulo
  | TripleEqual -> TripleEqual
  | NotTripleEqual -> NotTripleEqual
  | Greater -> Greater
  | Less -> Less
  | GreaterEqual -> GreaterEqual
  | LessEqual -> LessEqual

let buitin_of_unary_operator: unary_operator -> builtin = function
  | Not -> Not
  | Negate -> Negate

let buitin_of_logical_operator: logical_operator -> builtin = function
  | And -> And
  | Or -> Or

let is_mark_i instr = (match instr with 
  | MarkInstr -> true
  | _ -> false)

let is_env_i instr = (match instr with 
  | EnvInstr _ -> true
  | _ -> false)

let is_reset_i instr = (match instr with 
  | ResetInstr -> true
  | _ -> false)

(* Value to string conversion for display *)
let rec string_of_value = function
  | Number n -> string_of_float n
  | String s -> "\"" ^ s ^ "\""
  | Boolean b -> string_of_bool b
  | Undefined -> "undefined"
  | Null -> "null"
  | Closure _ -> "<closure>"
  | Builtin b -> "<builtin " ^ string_of_builtin b ^ ">"
  | Continuation _ -> "<continuation>"
  | UnassignedValue -> "<unassigned>"
  | Pair { fst; snd } ->
      "[" ^ string_of_value fst ^ ", " ^ string_of_value snd ^ "]"
  | Handler _ -> "<handler>"
  | ResetStashMarker -> "<reset_stash_marker>"

let string_of_command cmd =
  match cmd with
  | Expression expr -> "Expression: " ^ (string_of_expression expr)
  | Statement stmt -> "Statement: " ^ (string_of_statement stmt)
  | PopInstr -> "PopInstr"
  | LitInstr v -> "LitInstr: " ^ (string_of_value v)
  | UnopInstr op -> "UnopInstr: " ^ (string_of_builtin op)
  | BinopInstr op -> "BinopInstr: " ^ (string_of_builtin op)
  | AppInstr n -> "AppInstr: " ^ (string_of_int n)
  | AssignmentInstr name -> "AssignmentInstr: " ^ name
  | BranchInstr { cons; alt } ->
      "BranchInstr: " ^ (string_of_statement cons) ^ " | " ^
      (string_of_statement alt)
  | WhileInstr { pred; body } ->
      "WhileInstr: " ^ (string_of_expression pred) ^ " | " ^
      (string_of_statement body)
  | EnvInstr _ -> "EnvInstr"
  | ResetInstr -> "ResetInstr"
  | MarkInstr -> "MarkInstr"
  | ResetControlMarker -> "ResetControlMarker"
  | PopHandlerInstr -> "PopHandlerInstr"
  | RunWithHandlerInstr _ -> "RunWithHandlerInstr"
  | PerformInstr (name, n) -> "PerformInstr: " ^ name ^ " | " ^
      (string_of_int n)
