(* interpreter.ml - Interpreter for Source §1 *)

open Ast

(* Value type for runtime values *)
type value =
  | NumberVal of float          (* Using float for all numbers *)
  | BooleanVal of bool          (* Boolean values *)
  | StringVal of string         (* String values *)
  | FunctionVal of function_val (* Function values *)
  | UndefinedVal                (* Undefined value *)

(* Function value representation *)
and function_val = {
  params: string list;           (* Parameter names *)
  body: lambda_body;             (* Function body *)
  closure: environment ref;      (* Lexical environment *)
}

(* Environment for storing variable bindings *)
and environment = {
  mutable bindings: ((string * value) list);            (* Current scope bindings *)
  parent: (environment ref) option;             (* Parent environment (for closures) *)
}

(* Environment operations *)

(* Global environment *)
let empty_env = { bindings = []; parent = None }

(* Look up a variable in the environment *)
let rec lookup_variable name env =
  try
    List.assoc name !env.bindings
  with Not_found ->
    match !env.parent with
    | Some parent -> lookup_variable name parent
    | None -> failwith ("Unbound variable: " ^ name)

(* Define a new variable in the current environment *)
let define_variable name value env =
  !env.bindings <- (name, value) :: !env.bindings

(* Extend environment with new bindings (for function calls) *)
let extend_env params args parent_env =
  let rec bind params args bindings =
    match params, args with
    | [], [] -> bindings
    | p::ps, a::as_ -> bind ps as_ ((p, a) :: bindings)
    | _, _ -> failwith "(internal) Parameter/argument mismatch"
  in
  { bindings = bind params args []; parent = Some parent_env }

(* Exception for return statements *)
exception Return of value

(* Convert OCaml values to string representation *)
let string_of_value = function
  | NumberVal n -> if n = Float.trunc n 
                   then string_of_int (int_of_float n)
                   else string_of_float n
  | BooleanVal b -> string_of_bool b
  | StringVal s -> "\"" ^ s ^ "\""
  | FunctionVal _ -> "<closure>"
  | UndefinedVal -> "undefined"

let pp_environment env =
  let rec pp_bindings = function
    | [] -> ""
    | (name, value) :: rest ->
        Printf.sprintf "%s = %s\n%s" name (string_of_value value) (pp_bindings rest)
  in
  let rec pp_env env =
    match !env.parent with
    | Some parent -> pp_env parent ^ " -> " ^ pp_bindings !env.bindings
    | None -> pp_bindings !env.bindings
  in
  pp_env env

(* Evaluation functions *)

(* Evaluate expressions *)
let rec eval_expression expr env =
  match expr with
  | NumberLiteral n -> NumberVal (float_of_int n)
  | BooleanLiteral b -> BooleanVal b
  | StringLiteral s -> StringVal s
  | NameExpression name -> lookup_variable name env
  
  | BinaryExpression (op, e1, e2) ->
      let v1 = eval_expression e1 env in
      let v2 = eval_expression e2 env in
      eval_binary_op op v1 v2
      
  | UnaryExpression (op, e) ->
      let v = eval_expression e env in
      eval_unary_op op v
      
  | LogicalExpression (And, e1, e2) ->
      let v1 = eval_expression e1 env in
      begin match v1 with
      | BooleanVal false -> BooleanVal false
      | _ -> eval_expression e2 env
      end

  | LogicalExpression (Or, e1, e2) ->
      let v1 = eval_expression e1 env in
      begin match v1 with
      | BooleanVal true -> BooleanVal true
      | _ -> eval_expression e2 env
      end
  
  | FunctionApplication (func_expr, args) ->
      let func_val = eval_expression func_expr env in
      let arg_vals = List.map (fun arg -> eval_expression arg env) args in
      apply_function func_val arg_vals env
      
  | LambdaExpression (params, body) ->
      FunctionVal { params; body; closure = env }
      
  | ConditionalExpression (cond, then_expr, else_expr) ->
      let cond_val = eval_expression cond env in
      match cond_val with
      | BooleanVal true -> eval_expression then_expr env
      | BooleanVal false -> eval_expression else_expr env
      | _ -> failwith "Condition must evaluate to a boolean"

(* Apply a function to arguments *)
and apply_function func args _ =
  match func with
  | FunctionVal { params; body; closure } ->
      let func_env = ref (extend_env params args closure) in
      begin match body with
      | ExprBody expr -> eval_expression expr func_env
      | BlockBody block -> 
          try
            let _ = eval_block block func_env in
            UndefinedVal
          with Return v -> v
      end
  | _ -> failwith "Not a function"

(* Evaluate binary operations *)
and eval_binary_op op v1 v2 =
  match op, v1, v2 with
  | Plus, NumberVal n1, NumberVal n2 -> NumberVal (n1 +. n2)
  | Plus, StringVal s1, StringVal s2 -> StringVal (s1 ^ s2)
  | Minus, NumberVal n1, NumberVal n2 -> NumberVal (n1 -. n2)
  | Times, NumberVal n1, NumberVal n2 -> NumberVal (n1 *. n2)
  | Divide, NumberVal n1, NumberVal n2 -> 
      if n2 = 0.0 then failwith "Division by zero"
      else NumberVal (n1 /. n2)
  | Modulo, NumberVal n1, NumberVal n2 -> 
      if n2 = 0.0 then failwith "Modulo by zero"
      else NumberVal (Float.rem n1 n2)
  | TripleEqual, NumberVal n1, NumberVal n2 -> BooleanVal (n1 = n2)
  | TripleEqual, StringVal s1, StringVal s2 -> BooleanVal (s1 = s2)
  | TripleEqual, BooleanVal b1, BooleanVal b2 -> BooleanVal (b1 = b2)
  | NotTripleEqual, NumberVal n1, NumberVal n2 -> BooleanVal (n1 <> n2)
  | NotTripleEqual, StringVal s1, StringVal s2 -> BooleanVal (s1 <> s2)
  | NotTripleEqual, BooleanVal b1, BooleanVal b2 -> BooleanVal (b1 <> b2)
  | Greater, NumberVal n1, NumberVal n2 -> BooleanVal (n1 > n2)
  | Greater, StringVal s1, StringVal s2 -> BooleanVal (s1 > s2)
  | Less, NumberVal n1, NumberVal n2 -> BooleanVal (n1 < n2)
  | Less, StringVal s1, StringVal s2 -> BooleanVal (s1 < s2)
  | GreaterEqual, NumberVal n1, NumberVal n2 -> BooleanVal (n1 >= n2)
  | GreaterEqual, StringVal s1, StringVal s2 -> BooleanVal (s1 >= s2)
  | LessEqual, NumberVal n1, NumberVal n2 -> BooleanVal (n1 <= n2)
  | LessEqual, StringVal s1, StringVal s2 -> BooleanVal (s1 <= s2)
  | _ ->
      failwith "Type error in binary operation"

(* Evaluate unary operations *)
and eval_unary_op op v =
  match op, v with
  | Not, BooleanVal b -> BooleanVal (not b)
  | Negate, NumberVal n -> NumberVal (-.n)
  | _ -> failwith "Type error in unary operation"

(* Execute statements *)
and eval_statement stmt env =
  match stmt with
  | ConstDeclaration (name, expr) ->
      let value = eval_expression expr env in
      define_variable name value env;
      env, UndefinedVal
      
  | FunctionDeclaration (name, params, body) ->
      let func_val = FunctionVal { params; body = BlockBody body; closure = env } in
      define_variable name func_val env;
      env, UndefinedVal
      
  | ReturnStatement expr ->
      let value = eval_expression expr env in
      raise (Return value)
      
  | IfStatement (cond, then_block, else_block) ->
      let cond_val = eval_expression cond env in
      begin match cond_val with
      | BooleanVal true -> eval_block then_block env
      | BooleanVal false -> eval_block else_block env
      | _ -> failwith "Condition must evaluate to a boolean"
      end
      
  | IfElseIfStatement (cond, then_block, else_if) ->
      let cond_val = eval_expression cond env in
      begin match cond_val with
      | BooleanVal true -> eval_block then_block env
      | BooleanVal false -> 
          let _, result = eval_statement else_if env in
          env, result
      | _ -> failwith "Condition must evaluate to a boolean"
      end
      
  | BlockStatement block ->
      eval_block block env
      
  | ExpressionStatement expr ->
      let value = eval_expression expr env in
      env, value

(* Evaluate a block of statements *)
and eval_block (Block stmts) env =
  (* extend env *)
  let block_env = ref (extend_env [] [] env) in
  let _, final_value = 
    List.fold_left 
      (fun (curr_env, _) stmt -> eval_statement stmt curr_env)
      (block_env, UndefinedVal)
      stmts
  in
  env, final_value  (* Return the original environment, not the block's environment *)

(* Evaluate a program *)
let eval_program (Program stmts) =
  try
    let _, value = 
      List.fold_left 
        (fun (env, _) stmt -> 
          eval_statement stmt env)
        (ref empty_env, UndefinedVal)
        stmts
    in
    value
  with
  | Return value -> value  (* Handle return outside of function *)
  | Failure msg -> 
      Printf.eprintf "Runtime error: %s\n" msg;
      UndefinedVal
