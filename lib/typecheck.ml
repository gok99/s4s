open Ast_typed

(* Type system implementation *)

(* Special answer type for pure expressions *)
let delta_answer_type = "Delta"

(* Environment for type checking *)
type environment = {
  type_env: (string * type_expr) list;  (* Type environment *)
  answer_env: (string * string) list;   (* Answer type environment *)
}

(* Helper functions *)
let lookup_type name env =
  try List.assoc name env.type_env
  with Not_found -> failwith ("Type error: undefined variable " ^ name)

let lookup_answer name env =
  try List.assoc name env.answer_env
  with Not_found -> delta_answer_type  (* Default to delta for pure expressions *)

let extend_typ_env name typ env =
  {
    env with 
    type_env = (name, typ) :: env.type_env;
  }

let extend_ans_type_env name ans env =
  {
    env with 
    answer_env = (name, ans) :: env.answer_env;
  }

let extend_env name typ ans env =
  {
    type_env = (name, typ) :: env.type_env;
    answer_env = (name, ans) :: env.answer_env;
  }

let extend_env_multiple names types answers env =
  let rec extend acc names types answers =
    match names, types, answers with
    | [], [], [] -> acc
    | n::ns, t::ts, a::as_ -> extend (extend_env n t a acc)
      ns ts as_
    | _, _, _ -> failwith "Type error: mismatched parameter lists"
  in
  extend env names types answers

let flatten_return_type = function
  | ReturnType t -> t
  | t -> t

(* Check if types are equal (if only one is an answer type, answer type is ignored!) *)
let rec equal_type t1 t2 =
  match t1, t2 with
  | SimpleType s1, SimpleType s2 -> s1 = s2
  | AnswerType (r1, a1), AnswerType (r2, a2) -> r1 = r2 && a1 = a2
  | SimpleType _, AnswerType (r1, _) -> equal_type t1 r1
  | AnswerType (r1, _), SimpleType _ -> equal_type r1 t2
  | FunctionType (params1, ret1), FunctionType (params2, ret2) ->
      equal_type ret1 ret2 && 
      (try List.for_all2 equal_type params1 params2
       with Invalid_argument _ -> false)
  | _, _ -> false

(* Determine if an answer type is compatible with answer types *)
let compatible_answer alpha1 alpha2 =
  alpha1 = alpha2 || alpha1 = delta_answer_type || alpha2 = delta_answer_type

(* Combine answer types - implements the ρ function from the spec *)
let combine_answers answers =
  let rec find_non_delta = function
    | [] -> delta_answer_type
    | a::rest -> if a <> delta_answer_type then a else find_non_delta rest
  in
  let non_delta = find_non_delta answers in
  
  (* Check that all non-delta answers are the same *)
  let all_compatible = List.for_all (fun a -> a = delta_answer_type || a = non_delta) answers in
  if all_compatible then non_delta
  else failwith "Type error: incompatible answer types"

(* Type checker *)
let rec type_check_expr expr env =
  match expr with
  | NumberLiteral _ -> (SimpleType "Number", delta_answer_type)
  | BooleanLiteral _ -> (SimpleType "Boolean", delta_answer_type)
  | StringLiteral _ -> (SimpleType "String", delta_answer_type)
  | NameExpression name ->
      let ans = lookup_answer name env in
      let ty = match (lookup_type name env) with
      | SimpleType s -> SimpleType s
      | FunctionType (params, ret) -> FunctionType (params, ret)
      | AnswerType (t, s) -> if s = ans
          then t
          else failwith ("Type error: answer type mismatch for " ^ name)
      | ReturnType _ -> failwith ("Name:" ^ name ^ "has unexpected 'return' type")
      in
      (ty, ans)

  | BinaryExpression (op, e1, e2) ->
      let (t1, a1) = type_check_expr e1 env in
      let (t2, a2) = type_check_expr e2 env in
      (* Printf.printf "t1: %s\n" (string_of_type t1);
      Printf.printf "t2: %s\n" (string_of_type t2); *)
      (* Check binary operator compatibility *)
      let result_type = match op, t1, t2 with
        | Plus, SimpleType "Number", SimpleType "Number" -> SimpleType "Number"
        | Plus, SimpleType "String", SimpleType "String" -> SimpleType "String"
        | Minus, SimpleType "Number", SimpleType "Number" -> SimpleType "Number"
        | Times, SimpleType "Number", SimpleType "Number" -> SimpleType "Number"
        | Divide, SimpleType "Number", SimpleType "Number" -> SimpleType "Number"
        | TripleEqual, _, _ when equal_type t1 t2 -> SimpleType "Boolean"
        | NotTripleEqual, _, _ when equal_type t1 t2 -> SimpleType "Boolean"
        | Less, SimpleType "Number", SimpleType "Number" -> SimpleType "Boolean"
        | LessEqual, SimpleType "Number", SimpleType "Number" -> SimpleType "Boolean"
        | Greater, SimpleType "Number", SimpleType "Number" -> SimpleType "Boolean"
        | GreaterEqual, SimpleType "Number", SimpleType "Number" -> SimpleType "Boolean"
        | _, _, _ -> failwith "Type error: invalid binary operation"
      in
      (result_type, combine_answers [a1; a2])
  
  | UnaryExpression (op, e) ->
      let (t, a) = type_check_expr e env in
      let result_type = match op, t with
        | Negate, SimpleType "Number" -> SimpleType "Number"
        | Not, SimpleType "Boolean" -> SimpleType "Boolean"
        | _, _ -> failwith "Type error: invalid unary operation"
      in
      (result_type, a)
  
  | LogicalExpression (_, e1, e2) ->
      let (t1, a1) = type_check_expr e1 env in
      let (t2, a2) = type_check_expr e2 env in
      begin match t1, t2 with
      | SimpleType "Boolean", SimpleType "Boolean" -> 
          (SimpleType "Boolean", combine_answers [a1; a2])
      | _, _ -> failwith "Type error: logical expressions must have boolean operands"
      end
  
  | ConditionalExpression (cond, then_expr, else_expr) ->
      let (cond_type, a1) = type_check_expr cond env in
      let (then_type, a2) = type_check_expr then_expr env in
      let (else_type, a3) = type_check_expr else_expr env in
      begin match cond_type with
      | SimpleType "Boolean" ->
          if equal_type then_type else_type then
            (then_type, combine_answers [a1; a2; a3])
          else
            failwith "Type error: branches of conditional must have same type"
      | _ -> failwith "Type error: condition must be boolean"
      end
  
  | TypedLambdaExpression (params, body) ->
      let param_names = List.map fst params in
      let param_temp_types = List.map (fun (_, ty) ->
        match ty with
        | AnswerType (a, b) -> (a, b)
        | SimpleType _ | FunctionType _ -> (ty, "Delta")
        | _ -> failwith "Type errors: typed function parameters must have answer types"
      ) params in
      
      let param_types = List.map fst param_temp_types in
      let param_answers = List.map snd param_temp_types in
      let extended_env = extend_env_multiple param_names param_types param_answers env in
      
      let (ret_type, body_answer) = type_check_lambda_body body extended_env in
      let combined_answer = combine_answers (body_answer :: param_answers) in

      let func_type = FunctionType (
        List.map (fun (t, a) -> AnswerType (t, a)) param_temp_types,
        flatten_return_type ret_type
      ) in
      
      (func_type, combined_answer)
  
  | FunctionApplication (func, args) ->
      (* Special handling for shift and reset *)
      match func with
      | NameExpression "shift" ->
          type_check_shift args env
      | NameExpression "reset" ->
          type_check_reset args env
      | _ ->
          (* Regular function application *)
          (* Printf.printf "Expression: %s\n" (string_of_expression func); *)
          let (func_type, func_answer) = type_check_expr func env in
          let args_with_types = List.map (fun e -> type_check_expr e env) args in
          let arg_types = List.map (fun (t, _) -> t) args_with_types in
          let arg_answers = List.map (fun (_, a) -> a) args_with_types in
          
          begin match func_type with
          | FunctionType (param_types, ret_type) ->
              (* Check parameter types match argument types *)
              if List.length param_types <> List.length arg_types then
                failwith "Type error: wrong number of arguments"
              else
                let param_temp_types = List.map (fun ty ->
                  match ty with | AnswerType (t, a) -> (t, a) | t -> (t, "Delta")) param_types 
                in
                let param_types = List.map fst param_temp_types in
                let param_answers = List.map snd param_temp_types in

                List.iter2 
                  (fun pt at -> 
                    if not (equal_type pt at) then
                      failwith "Type error: argument type mismatch") 
                  param_types arg_types;
                List.iter2 
                  (fun pt at -> 
                    if not (at = "Delta" || pt = at) then
                      failwith "Type error: argument answer type mismatch") 
                  param_answers arg_answers;
                
                (* Return function's return type and combined answer types *)
                (ret_type, combine_answers (func_answer :: arg_answers))
          | _ -> failwith "Type error: expression is not callable"
          end

and type_check_lambda_body body env =
  match body with
  | ExprBody expr -> type_check_expr expr env
  | BlockBody block -> type_check_block block env

and type_check_shift args env =
  let check_shift_f k_type body_type body_answer =
    let k_param_type = match k_type with
      | AnswerType (FunctionType ([param_type], _), _)
      | FunctionType ([param_type], _) 
        -> param_type
      | _ -> failwith "Type error: shift continuation must be a function with one parameter"
    in
    begin match body_type with
    | SimpleType s when compatible_answer s body_answer ->
      k_param_type, s
    | _ -> failwith "Type error: shift body must have same type as its answer type"
    end
  in
  match args with
  | [e] ->
    let (func_type, body_answer) = type_check_expr e env in
    begin match func_type with
    | FunctionType ([k_type], body_type) -> check_shift_f k_type body_type body_answer
    | _ -> failwith "Type error: shift continuation must be a function"
    end
  | _ -> failwith "Type error: shift takes exactly one argument"

and type_check_reset args env =
  match args with
  | [TypedLambdaExpression ([], body)] ->
      let (body_type, body_answer) = type_check_lambda_body body env in
      
      (* For reset, answer type must be same as expression type *)
      begin match body_type with
      | SimpleType s ->
          if s = body_answer || body_answer = delta_answer_type then
            (body_type, delta_answer_type) (* Reset returns to pure *)
          else
              failwith "Type error: reset body must have same type as its answer type"
      | _ -> failwith "Type error: reset body must have simple type"
      end

  | _ -> failwith "Type error: reset requires a nullary lambda"

and type_check_block (Block stmts) env =
  let (t, a, _) = type_check_statements stmts env in
  (t, a)

and type_check_statements stmts env =
  match stmts with
  | [] -> (SimpleType "Undefined", delta_answer_type, env)
  | [stmt] -> type_check_statement stmt env
  | stmt :: rest ->
      let (t1, a1, updated_env) = type_check_statement stmt env in
      match t1 with
      | ReturnType t1 -> (t1, a1, updated_env) (* Return statement terminates *)
      | _ -> 
          let (t2, a2, env) = type_check_statements rest updated_env in
          (t2, combine_answers [a1; a2], env)

and type_check_statement stmt env =
  match stmt with
  | ConstDeclaration (name, expr) ->
      let (expr_type, expr_answer) = type_check_expr expr env in
      (SimpleType "Undefined", expr_answer, extend_env name expr_type expr_answer env)
  
  | TypedConstDeclaration (name, type_ann, expr) ->
      let (expr_type, expr_answer) = type_check_expr expr env in
      if equal_type expr_type type_ann then
        (SimpleType "Undefined", expr_answer, extend_env name type_ann expr_answer env)
      else
        failwith ("Type error: expression type doesn't match annotation for " ^ name)
  
  | FunctionDeclaration (name, params, ret_type, block) ->
      let param_names = List.map (fun (n, _) -> n) params in
      let param_temp_types = List.map (fun (_, t) -> 
        match t with
        | AnswerType (a, b) -> (a, b)
        | SimpleType _ | FunctionType _ -> (t, "Delta")
        | _ -> failwith "Type error: typed function parameters must have answer types") params
      in
      let param_types = List.map fst param_temp_types in
      let param_answers = List.map snd param_temp_types in
      let func_type = FunctionType (
        List.map (fun (t, a) -> AnswerType (t, a)) param_temp_types,
        flatten_return_type ret_type)
      in
      
      let extended_env = extend_env_multiple (name::param_names) (func_type::param_types) (delta_answer_type::param_answers) env in
      let (block_type, block_answer) = type_check_block block extended_env in

      let new_env = extend_env name func_type block_answer env in
      
      (* Check return type matches function declaration *)
      (match block_type with
       | ReturnType block_type ->
           if equal_type ret_type block_type then
             (SimpleType "Undefined", block_answer, new_env)
           else
             failwith "Type error: function body return type doesn't match declaration"
       | _ ->
           if equal_type ret_type (SimpleType "Undefined") then
             (SimpleType "Undefined", block_answer, new_env)
           else
             failwith "Type error: function body must return a value matching declaration")
  
  | ReturnStatement expr ->
      let (expr_type, expr_answer) = type_check_expr expr env in
      (ReturnType expr_type, expr_answer, env)
  
  | IfStatement (cond, then_block, else_block) ->
      let (cond_type, cond_answer) = type_check_expr cond env in
      let (then_type, then_answer) = type_check_block then_block env in
      let (else_type, else_answer) = type_check_block else_block env in
      
      begin match cond_type with
      | SimpleType "Boolean" ->
          if equal_type then_type else_type then
            (then_type, combine_answers [cond_answer; then_answer; else_answer], env)
          else
            failwith "Type error: branches of if statement must have same type"
      | _ -> failwith "Type error: if condition must be boolean"
      end
  
  | IfElseIfStatement (cond, then_block, else_stmt) ->
      let (cond_type, cond_answer) = type_check_expr cond env in
      let (then_type, then_answer) = type_check_block then_block env in
      let (else_type, else_answer, _) = type_check_statement else_stmt env in
      
      begin match cond_type with
      | SimpleType "Boolean" ->
          if equal_type then_type else_type then
            (then_type, combine_answers [cond_answer; then_answer; else_answer], env)
          else
            failwith "Type error: branches of if-else-if statement must have same type"
      | _ -> failwith "Type error: if condition must be boolean"
      end
  
  | BlockStatement block ->
      let (t, a) = type_check_block block env in
      (t, a, env)
  
  | ExpressionStatement expr ->
      let (expr_type, expr_answer) = type_check_expr expr env in
      (expr_type, expr_answer, env)

let pp_env env =
  Printf.printf "==Type Environment==\n";
  List.iter (fun (name, typ) -> Printf.printf "%s: %s\n" name (string_of_type typ)) env.type_env;
  Printf.printf "\n==Answer Type Environment==\n";
  List.iter (fun (name, ans) -> Printf.printf "%s: %s\n" name ans) env.answer_env;
  Printf.printf "============================\n\n"

(* Main entry point for type checking a program *)
let type_check (Program stmts) =
  let initial_env = { type_env = []; answer_env = [] } in
  let (prog_type, prog_answer, env) = type_check_statements stmts initial_env in
  pp_env env;
  (prog_type, prog_answer)
