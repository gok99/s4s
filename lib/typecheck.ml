(* type.ml - Types and type inference for Source §1
open Ast

(* Type definitions *)
type ty = 
  | TInt                        (* Integer *)
  | TBool                       (* Boolean *)
  | TString                     (* String *)
  | TFunc of ty list * ty       (* Function: parameters -> return type *)
  | TVar of int                 (* Type variable *)

let rec pp_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TFunc(args, ret) -> 
      "(" ^ String.concat ", " (List.map pp_ty args) ^ ") -> " ^ pp_ty ret
  | TVar id -> "t" ^ string_of_int id

(* Environment maps variables to their types *)
type env = (string * ty) list

(* Constraints are equalities between types *)
type constraint_t = ty * ty

(* State for fresh type variables *)
let next_var_id = ref 0
let fresh_var () =
  let id = !next_var_id in
  next_var_id := id + 1;
  TVar id

(* Substitution maps type variables to types *)
type subst = (int * ty) list

(* Apply a substitution to a type *)
let rec apply_subst (s: subst) (t: ty) : ty =
  match t with
  | TInt | TBool | TString -> t
  | TFunc(args, ret) -> TFunc(List.map (apply_subst s) args, apply_subst s ret)
  | TVar id ->
      match List.assoc_opt id s with
      | Some ty -> apply_subst s ty  (* Continue applying in case of nested substitutions *)
      | None -> t

(* Apply a substitution to a list of constraints *)
let apply_subst_to_constraints (s: subst) (cs: constraint_t list) : constraint_t list =
  List.map (fun (t1, t2) -> (apply_subst s t1, apply_subst s t2)) cs

(* Check if a type variable occurs in a type (occurs check) *)
let rec occurs (id: int) (t: ty) : bool =
  match t with
  | TInt | TBool | TString -> false
  | TFunc(args, ret) -> List.exists (occurs id) args || occurs id ret
  | TVar id' -> id = id'

(* Compose two substitutions *)
let compose_subst (s1: subst) (s2: subst) : subst =
  let s1' = List.map (fun (id, t) -> (id, apply_subst s2 t)) s1 in
  let s2_filtered = List.filter (fun (id, _) -> not (List.mem_assoc id s1)) s2 in
  s1' @ s2_filtered

(* Unify two types, returning a substitution *)
let rec unify (t1: ty) (t2: ty) : subst =
  match t1, t2 with
  | TInt, TInt | TBool, TBool | TString, TString -> []
  | TFunc(args1, ret1), TFunc(args2, ret2) when List.length args1 = List.length args2 ->
      let s1 = unify_list args1 args2 [] in
      let s2 = unify (apply_subst s1 ret1) (apply_subst s1 ret2) in
      compose_subst s1 s2
  | TVar id, t | t, TVar id ->
      if t = TVar id then []
      else if occurs id t then failwith "Occurs check failed: infinite type"
      else [(id, t)]
  | _, _ -> failwith ("Type mismatch: " ^ string_of_type t1 ^ " != " ^ string_of_type t2)

(* Unify a list of types, accumulating a substitution *)
and unify_list (ts1: ty list) (ts2: ty list) (s: subst) : subst =
  match ts1, ts2 with
  | [], [] -> s
  | t1::ts1', t2::ts2' ->
      let s' = unify (apply_subst s t1) (apply_subst s t2) in
      unify_list ts1' ts2' (compose_subst s s')
  | _, _ -> failwith "Mismatched number of arguments in function type unification"

(* Unify a list of constraints, returning a substitution *)
and unify_constraints (cs: constraint_t list) : subst =
  match cs with
  | [] -> []
  | (t1, t2)::cs' ->
      let s1 = unify t1 t2 in
      let cs'' = apply_subst_to_constraints s1 cs' in
      let s2 = unify_constraints cs'' in
      compose_subst s1 s2

(* String representation of types for debugging *)
and string_of_type (t: ty) : string =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TFunc(args, ret) -> 
      "(" ^ String.concat ", " (List.map string_of_type args) ^ ") -> " ^ string_of_type ret
  | TVar id -> "t" ^ string_of_int id

(* Apply environment lookup with default to fresh type variable *)
let lookup_env (env: env) (name: string) : ty =
  match List.assoc_opt name env with
  | Some t -> t
  | None -> failwith ("Unbound variable: " ^ name)

(* Generate typing constraints for binary operations *)
let type_of_binary_op (op: binary_operator) : ty * ty * ty =
  match op with
  | Plus -> (TInt, TInt, TInt)
  (*| Plus -> (TString, TString, TString)  (* Overloaded for strings *) *)
  | Minus | Times | Divide | Modulo -> (TInt, TInt, TInt)
  | TripleEqual | NotTripleEqual -> 
      let tv = fresh_var() in (tv, tv, TBool)  (* Can compare any type with itself *)
  | Greater | Less | GreaterEqual | LessEqual -> 
      let tv = fresh_var() in
      match tv with
      | TVar _ -> (TInt, TInt, TBool)  (* Default to numeric comparison *)
      | _ -> (tv, tv, TBool)

(* Generate typing constraints for unary operations *)
let type_of_unary_op (op: unary_operator) : ty * ty =
  match op with
  | Not -> (TBool, TBool)
  | Negate -> (TInt, TInt)

(* Generate constraints for an expression *)
let rec generate_expr_constraints (env: env) (expr: expression) : ty * constraint_t list =
  match expr with
  | NumberLiteral _ -> (TInt, [])
  | BooleanLiteral _ -> (TBool, [])
  | StringLiteral _ -> (TString, [])
  
  | NameExpression name ->
      (try (lookup_env env name, [])
       with Failure _ -> (fresh_var(), []))  (* Create a fresh type var for undefined names *)
  
  | BinaryExpression(op, e1, e2) ->
      let (t1, c1) = generate_expr_constraints env e1 in
      let (t2, c2) = generate_expr_constraints env e2 in
      let (expected_t1, expected_t2, result_t) = type_of_binary_op op in
      (result_t, c1 @ c2 @ [(t1, expected_t1); (t2, expected_t2)])
  
  | UnaryExpression(op, e) ->
      let (t, c) = generate_expr_constraints env e in
      let (expected_t, result_t) = type_of_unary_op op in
      (result_t, c @ [(t, expected_t)])
  
  | LogicalExpression(op, e1, e2) ->
      let (t1, c1) = generate_expr_constraints env e1 in
      let (_, c2) = generate_expr_constraints env e2 in
      (match op with
      | And | Or -> (TBool, c1 @ c2 @ [(t1, TBool)]))  (* Only first arg must be boolean *)
  
  | FunctionApplication(func, args) ->
      let (func_t, func_c) = generate_expr_constraints env func in
      let arg_types_and_constraints = List.map (generate_expr_constraints env) args in
      let arg_types = List.map fst arg_types_and_constraints in
      let arg_constraints = List.concat (List.map snd arg_types_and_constraints) in
      let return_t = fresh_var() in
      let expected_func_t = TFunc(arg_types, return_t) in
      (return_t, func_c @ arg_constraints @ [(func_t, expected_func_t)])
  
  | LambdaExpression(params, body) ->
      let param_types = List.map (fun _ -> fresh_var()) params in
      let param_env = List.combine params param_types @ env in
      
      let (body_t, body_c) = match body with
        | ExprBody e -> generate_expr_constraints param_env e
        | BlockBody b -> 
            let (block_t, block_c) = generate_block_constraints param_env b in
            (block_t, block_c)
      in
      
      (TFunc(param_types, body_t), body_c)
  
  | ConditionalExpression(cond, then_expr, else_expr) ->
      let (cond_t, cond_c) = generate_expr_constraints env cond in
      let (then_t, then_c) = generate_expr_constraints env then_expr in
      let (else_t, else_c) = generate_expr_constraints env else_expr in
      let result_t = fresh_var() in
      (result_t, cond_c @ then_c @ else_c @ [(cond_t, TBool); (then_t, result_t); (else_t, result_t)])

(* Generate constraints for a block *)
and generate_block_constraints (env: env) (Block stmts: block) : ty * constraint_t list =
  let rec process_stmts env stmts return_type_opt constraints =
    match stmts with
    | [] -> 
        (match return_type_opt with
         | Some t -> (t, constraints)
         | None -> (TVar(-1), constraints))  (* Void return if no return statement *)
    
    | stmt :: rest ->
        let (stmt_t, stmt_c, new_env, is_return) = generate_stmt_constraints env stmt in
        if is_return then
          match return_type_opt with
          | None -> (stmt_t, constraints @ stmt_c)
          | Some prev_ret_t -> 
              (* Multiple returns must have same type *)
              (prev_ret_t, constraints @ stmt_c @ [(stmt_t, prev_ret_t)])
        else
          process_stmts new_env rest return_type_opt (constraints @ stmt_c)
  in
  
  process_stmts env stmts None []

(* Generate constraints for a statement, returns (type, constraints, new_env, is_return) *)
and generate_stmt_constraints (env: env) (stmt: statement) 
    : ty * constraint_t list * env * bool =
  match stmt with
  | ConstDeclaration(name, expr) ->
      let (expr_t, expr_c) = generate_expr_constraints env expr in
      (expr_t, expr_c, (name, expr_t) :: env, false)
  
  | FunctionDeclaration(name, params, body) ->
      let param_types = List.map (fun _ -> fresh_var()) params in
      let return_t = fresh_var() in
      let func_t = TFunc(param_types, return_t) in
      
      let extended_env = (name, func_t) :: env in
      let param_env = List.combine params param_types @ extended_env in
      
      let (body_t, body_c) = generate_block_constraints param_env body in
      
      (func_t, body_c @ [(body_t, return_t)], extended_env, false)
  
  | ReturnStatement(expr) ->
      let (expr_t, expr_c) = generate_expr_constraints env expr in
      (expr_t, expr_c, env, true)
  
  | IfStatement(cond, then_block, else_block) ->
      let (cond_t, cond_c) = generate_expr_constraints env cond in
      let (then_t, then_c) = generate_block_constraints env then_block in
      let (else_t, else_c) = generate_block_constraints env else_block in
      (fresh_var(), cond_c @ then_c @ else_c @ [(cond_t, TBool); (then_t, else_t)], env, false)
  
  | IfElseIfStatement(cond, then_block, else_stmt) ->
      let (cond_t, cond_c) = generate_expr_constraints env cond in
      let (then_t, then_c) = generate_block_constraints env then_block in
      let (else_t, else_c, _, _) = generate_stmt_constraints env else_stmt in
      (fresh_var(), cond_c @ then_c @ else_c @ [(cond_t, TBool); (then_t, else_t)], env, false)
  
  | BlockStatement(block) ->
      let (block_t, block_c) = generate_block_constraints env block in
      (block_t, block_c, env, false)  (* Block doesn't introduce new scope in the enclosing scope *)
  
  | ExpressionStatement(expr) ->
      let (expr_t, expr_c) = generate_expr_constraints env expr in
      (expr_t, expr_c, env, false)

(* Top-level type inference for a program *)
let infer_program (Program stmts: program) : (string * ty) list =
  let rec process_stmts env stmts constraints =
    match stmts with
    | [] -> (env, constraints)
    | stmt :: rest ->
        let (_, stmt_c, new_env, _) = generate_stmt_constraints env stmt in
        process_stmts new_env rest (constraints @ stmt_c)
  in
  
  (* Reset type variable counter *)
  next_var_id := 0;
  
  (* Generate constraints for the whole program *)
  let (env, constraints) = process_stmts [] stmts [] in
  
  (* Solve constraints *)
  let substitution = 
    try unify_constraints constraints
    with Failure msg -> 
      failwith ("Type inference failed: " ^ msg)
  in
  
  (* Apply substitution to environment types *)
  List.map (fun (name, t) -> (name, apply_subst substitution t)) env

(* Utility function for type checking an expression directly *)
let type_check_expr (env: env) (expr: expression) : ty =
  let (t, constraints) = generate_expr_constraints env expr in
  let substitution = unify_constraints constraints in
  apply_subst substitution t

(* Type check a complete program *)
let type_check (prog: program) : (string * ty) list =
  try infer_program prog
  with Failure msg ->
    failwith ("Type checking failed: " ^ msg) *)
