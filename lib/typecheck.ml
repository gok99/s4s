(* type_checker.ml - Type checker for Source §4 with Delimited Control *)
open Ast

(* Types in our system *)
type ty =
  | TyNumber
  | TyBoolean
  | TyString
  | TyFunction of ty list * effect * ty * effect
  | TyHandler of effect * effect  (* Transforms one effect into another *)
  | TyVar of int  (* For type variables in polymorphic types *)
  | TyVoid  (* For statements that don't return a value *)

(* Effects for tracking control flow *)
and effect =
  | Pure  (* No effects *)
  | Effect of string * ty list * effect  (* Named effect with parameters and remaining effects *)
  | EffectVar of int  (* For effect variables in polymorphic effects *)

(* Type environment mapping variables to their types *)
type type_env = (string * ty) list

(* Effect environment mapping handler names to their effect signatures *)
type effect_env = (string * (ty list * ty)) list

(* Type checking error *)
exception TypeError of string

(* Fresh type variable generator *)
let next_type_var =
  let counter = ref 0 in
  fun () -> 
    let n = !counter in
    counter := n + 1;
    TyVar n

(* Fresh effect variable generator *)
let next_effect_var =
  let counter = ref 0 in
  fun () -> 
    let n = !counter in
    counter := n + 1;
    EffectVar n

(* Check if a type contains a particular type variable *)
let rec occurs (tv: int) (t: ty) : bool =
  match t with
  | TyNumber | TyBoolean | TyString | TyVoid -> false
  | TyVar n -> n = tv
  | TyFunction (args, eff_in, ret, eff_out) -> 
      List.exists (occurs tv) args || 
      occurs_effect tv eff_in || 
      occurs tv ret ||
      occurs_effect tv eff_out
  | TyHandler (eff_in, eff_out) ->
      occurs_effect tv eff_in || occurs_effect tv eff_out

(* Check if an effect contains a particular type variable *)
and occurs_effect (tv: int) (eff: effect) : bool =
  match eff with
  | Pure -> false
  | Effect (_, types, next_eff) -> 
      List.exists (occurs tv) types || occurs_effect tv next_eff
  | EffectVar _ -> false  (* Effect variables are separate from type variables *)

(* Unification of types *)
let rec unify (t1: ty) (t2: ty) : unit =
  match (t1, t2) with
  | (TyNumber, TyNumber) | (TyBoolean, TyBoolean) | (TyString, TyString) | (TyVoid, TyVoid) -> ()
  | (TyVar n, t) | (t, TyVar n) ->
      if occurs n t then
        raise (TypeError "Recursive type detected during unification")
      else
        () (* Here we would update the substitution in a more complete implementation *)
  | (TyFunction (args1, eff_in1, ret1, eff_out1), TyFunction (args2, eff_in2, ret2, eff_out2)) ->
      if List.length args1 <> List.length args2 then
        raise (TypeError "Function argument count mismatch")
      else
        begin
          List.iter2 unify args1 args2;
          unify_effect eff_in1 eff_in2;
          unify ret1 ret2;
          unify_effect eff_out1 eff_out2
        end
  | (TyHandler (eff_in1, eff_out1), TyHandler (eff_in2, eff_out2)) ->
      unify_effect eff_in1 eff_in2;
      unify_effect eff_out1 eff_out2
  | _ -> raise (TypeError "Type mismatch during unification")

(* Unification of effects *)
and unify_effect (e1: effect) (e2: effect) : unit =
  match (e1, e2) with
  | (Pure, Pure) -> ()
  | (Effect (name1, args1, next1), Effect (name2, args2, next2)) ->
      if name1 <> name2 || List.length args1 <> List.length args2 then
        raise (TypeError "Effect mismatch")
      else
        begin
          List.iter2 unify args1 args2;
          unify_effect next1 next2
        end
  | (EffectVar _, _) | (_, EffectVar _) ->
      () (* Here we would update the effect substitution *)
  | _ -> raise (TypeError "Effect mismatch during unification")

(* Look up a variable in the environment *)
let rec lookup_var (name: string) (env: type_env) : ty =
  match env with
  | [] -> raise (TypeError ("Unbound variable: " ^ name))
  | (var_name, var_type) :: rest ->
      if var_name = name then var_type else lookup_var name rest

(* Look up an effect in the environment *)
let rec lookup_effect (name: string) (env: effect_env) : ty list * ty =
  match env with
  | [] -> raise (TypeError ("Unbound effect: " ^ name))
  | (eff_name, eff_sig) :: rest ->
      if eff_name = name then eff_sig else lookup_effect name rest

(* Type check a binary operation *)
let type_check_binary_op (op: binary_operator) (t1: ty) (t2: ty) : ty =
  match op with
  | Plus | Minus | Times | Divide | Modulo ->
      unify t1 TyNumber;
      unify t2 TyNumber;
      TyNumber
  | TripleEqual | NotTripleEqual ->
      unify t1 t2;  (* Operands must have the same type *)
      TyBoolean
  | Greater | Less | GreaterEqual | LessEqual ->
      unify t1 TyNumber;
      unify t2 TyNumber;
      TyBoolean

(* Type check a unary operation *)
let type_check_unary_op (op: unary_operator) (t: ty) : ty =
  match op with
  | Not -> 
      unify t TyBoolean;
      TyBoolean
  | Negate -> 
      unify t TyNumber;
      TyNumber

(* Type check a logical operation *)
let type_check_logical_op (_: logical_operator) (t1: ty) (t2: ty) : ty =
  unify t1 TyBoolean;
  unify t2 TyBoolean;
  TyBoolean

(* Type check an expression *)
let rec type_check_expr (expr: expression) (env: type_env) (eff_env: effect_env) 
    (expected_eff: effect) : ty * effect =
  match expr with
  | NumberLiteral _ -> (TyNumber, expected_eff)
  | BooleanLiteral _ -> (TyBoolean, expected_eff)
  | StringLiteral _ -> (TyString, expected_eff)
  | NameExpression name -> (lookup_var name env, expected_eff)
  
  | BinaryExpression (op, e1, e2) ->
      let (t1, eff1) = type_check_expr e1 env eff_env expected_eff in
      let (t2, eff2) = type_check_expr e2 env eff_env eff1 in
      (type_check_binary_op op t1 t2, eff2)
  
  | UnaryExpression (op, e) ->
      let (t, eff) = type_check_expr e env eff_env expected_eff in
      (type_check_unary_op op t, eff)
  
  | LogicalExpression (op, e1, e2) ->
      let (t1, eff1) = type_check_expr e1 env eff_env expected_eff in
      let (t2, eff2) = type_check_expr e2 env eff_env eff1 in
      (type_check_logical_op op t1 t2, eff2)
  
  | FunctionApplication (func, args) ->
      let (func_type, func_eff) = type_check_expr func env eff_env expected_eff in
      
      (* Process arguments from left to right, threading the effect *)
      let (arg_types, final_eff) = 
        List.fold_left (fun (types, current_eff) arg ->
          let (arg_type, next_eff) = type_check_expr arg env eff_env current_eff in
          (types @ [arg_type], next_eff)
        ) ([], func_eff) args 
      in
      
      (* The function should be a function type *)
      (match func_type with
      | TyFunction (param_types, eff_in, ret_type, eff_out) ->
          if List.length param_types <> List.length arg_types then
            raise (TypeError "Wrong number of arguments in function application")
          else
            begin
              (* Unify parameter types with argument types *)
              List.iter2 unify param_types arg_types;
              
              (* Ensure the input effect matches the current effect *)
              unify_effect eff_in final_eff;
              
              (* Return the function's return type and output effect *)
              (ret_type, eff_out)
            end
      | _ -> raise (TypeError "Expected a function in application"))
  
  | LambdaExpression (params, body) ->
      (* Create fresh effect variables for the function's effect signature *)
      let eff_in = next_effect_var () in
      let eff_out = next_effect_var () in
      
      (* Extend environment with parameter types (using fresh type variables) *)
      let param_types = List.map (fun _ -> next_type_var ()) params in
      let extended_env = 
        List.fold_left2 
          (fun env param param_type -> (param, param_type) :: env) 
          env params param_types 
      in
      
      (* Type check the body with the extended environment *)
      let (ret_type, actual_eff_out) = 
        match body with
        | ExprBody e -> type_check_expr e extended_env eff_env eff_in
        | BlockBody b -> 
            let (block_type, block_eff) = 
              type_check_block b extended_env eff_env eff_in None in
            (block_type, block_eff)
      in
      
      (* Unify the actual output effect with the expected output effect *)
      unify_effect actual_eff_out eff_out;
      
      (* Return the function type *)
      (TyFunction (param_types, eff_in, ret_type, eff_out), expected_eff)
  
  | ConditionalExpression (cond, then_expr, else_expr) ->
      let (cond_type, cond_eff) = type_check_expr cond env eff_env expected_eff in
      unify cond_type TyBoolean;
      
      let (then_type, _) = type_check_expr then_expr env eff_env cond_eff in
      let (else_type, else_eff) = type_check_expr else_expr env eff_env cond_eff in
      
      (* Both branches must have the same type *)
      unify then_type else_type;
      
      (* Both branches should have compatible effects - for simplicity we'll use the else branch effect *)
      (then_type, else_eff)
  
  | WithHandler (handler_expr, body) ->
      (* Type check the handler *)
      let (handler_type, _) = type_check_expr handler_expr env eff_env expected_eff in
      
      (* The handler must be of type TyHandler *)
      (match handler_type with
      | TyHandler (eff_in, eff_out) ->
          (* Type check the body with the handler's input effect *)
          let (body_type, body_eff) = type_check_block body env eff_env eff_in None in
          
          (* The body's effect must be compatible with the handler's input effect *)
          unify_effect body_eff eff_in;
          
          (* The result type is the body's type with the handler's output effect *)
          (body_type, eff_out)
      | _ -> raise (TypeError "Expected a handler in with-handler expression"))
  
  | PerformExpression (eff_name, args) ->
      (* Look up the effect signature *)
      let (param_types, result_type) = lookup_effect eff_name eff_env in
      
      (* Check that we have the right number of arguments *)
      if List.length args <> List.length param_types then
        raise (TypeError "Wrong number of arguments in perform expression")
      else
        begin
          (* Type check each argument *)
          let (arg_types, final_eff) = 
            List.fold_left (fun (types, current_eff) arg ->
              let (arg_type, next_eff) = type_check_expr arg env eff_env current_eff in
              (types @ [arg_type], next_eff)
            ) ([], expected_eff) args 
          in
          
          (* Unify argument types with parameter types *)
          List.iter2 unify param_types arg_types;
          
          (* The result type is the effect's result type *)
          (* The effect is the current effect with this effect added *)
          (result_type, Effect (eff_name, arg_types, final_eff))
        end

(* Type check a block *)
and type_check_block (Block stmts) (env: type_env) (eff_env: effect_env) 
    (expected_eff: effect) (expected_ret: ty option) : ty * effect =
  (* Type check each statement in sequence, threading the environment and effect *)
  let rec check_stmts stmts env current_eff =
    match stmts with
    | [] -> 
        (* If no return statement was encountered, the block returns void *)
        (match expected_ret with
        | Some t -> (t, current_eff)  (* Return the expected return type *)
        | None -> (TyVoid, current_eff))
    | stmt :: rest ->
        let (stmt_type, stmt_env, stmt_eff) = type_check_stmt stmt env eff_env current_eff in
        
        (* If this is a return statement, the block returns this type *)
        (match stmt with
        | ReturnStatement _ -> 
            (* Check that the return type matches the expected return type, if any *)
            (match expected_ret with
            | Some t -> unify stmt_type t; (stmt_type, stmt_eff)
            | None -> (stmt_type, stmt_eff))
        | _ -> check_stmts rest stmt_env stmt_eff)
  in
  check_stmts stmts env expected_eff

(* Type check a statement *)
and type_check_stmt (stmt: statement) (env: type_env) (eff_env: effect_env) 
    (expected_eff: effect) : ty * type_env * effect =
  match stmt with
  | ConstDeclaration (name, expr) ->
      let (expr_type, expr_eff) = type_check_expr expr env eff_env expected_eff in
      (TyVoid, (name, expr_type) :: env, expr_eff)
  
  | LetDeclaration (name, expr) ->
      let (expr_type, expr_eff) = type_check_expr expr env eff_env expected_eff in
      (TyVoid, (name, expr_type) :: env, expr_eff)
  
  | AssignmentStatement (name, expr) ->
      let var_type = lookup_var name env in
      let (expr_type, expr_eff) = type_check_expr expr env eff_env expected_eff in
      unify var_type expr_type;
      (TyVoid, env, expr_eff)
  
  | FunctionDeclaration (name, params, body) ->
      (* Create fresh effect variables for the function's effect signature *)
      let eff_in = next_effect_var () in
      let eff_out = next_effect_var () in
      
      (* Create fresh type variables for parameters *)
      let param_types = List.map (fun _ -> next_type_var ()) params in
      
      (* Create the function type *)
      let ret_type = next_type_var () in
      let func_type = TyFunction (param_types, eff_in, ret_type, eff_out) in
      
      (* Extend environment with function name for recursive calls *)
      let extended_env = (name, func_type) :: env in
      
      (* Extend environment with parameter types *)
      let body_env = 
        List.fold_left2 
          (fun env param param_type -> (param, param_type) :: env) 
          extended_env params param_types 
      in
      
      (* Type check the body with the expected return type *)
      let (body_type, body_eff) = 
        type_check_block body body_env eff_env eff_in (Some ret_type) in
      
      (* Unify the return type with the body type *)
      unify ret_type body_type;
      
      (* Unify the output effect with the body effect *)
      unify_effect eff_out body_eff;
      
      (TyVoid, extended_env, expected_eff)
  
  | ReturnStatement expr ->
      let (expr_type, expr_eff) = type_check_expr expr env eff_env expected_eff in
      (expr_type, env, expr_eff)
  
  | IfStatement (cond, then_block, else_block) ->
      let (cond_type, cond_eff) = type_check_expr cond env eff_env expected_eff in
      unify cond_type TyBoolean;
      
      let (then_type, _) = type_check_block then_block env eff_env cond_eff None in
      let (else_type, else_eff) = type_check_block else_block env eff_env cond_eff None in
      
      (* Both branches must return the same type *)
      unify then_type else_type;
      
      (then_type, env, else_eff)
  
  | IfElseIfStatement (cond, then_block, else_if_stmt) ->
      let (cond_type, cond_eff) = type_check_expr cond env eff_env expected_eff in
      unify cond_type TyBoolean;
      
      let (then_type, _) = type_check_block then_block env eff_env cond_eff None in
      let (else_type, _, else_eff) = type_check_stmt else_if_stmt env eff_env cond_eff in
      
      (* Both branches must return the same type *)
      unify then_type else_type;
      
      (then_type, env, else_eff)
  
  | WhileStatement (cond, body) ->
      let (cond_type, cond_eff) = type_check_expr cond env eff_env expected_eff in
      unify cond_type TyBoolean;
      
      let (_, body_eff) = type_check_block body env eff_env cond_eff None in
      
      (* While loops return void *)
      (TyVoid, env, body_eff)
  
  | BreakStatement ->
      (* Break statements can only appear in loops (not checked here) *)
      (TyVoid, env, expected_eff)
  
  | ContinueStatement ->
      (* Continue statements can only appear in loops (not checked here) *)
      (TyVoid, env, expected_eff)
  
  | BlockStatement block ->
      let (block_type, block_eff) = type_check_block block env eff_env expected_eff None in
      (block_type, env, block_eff)
  
  | ExpressionStatement expr ->
      let (_, expr_eff) = type_check_expr expr env eff_env expected_eff in
      (TyVoid, env, expr_eff)
  
  | HandlerDeclaration (name, handlers) ->
      (* For each handler, check its signature *)
      let handler_sigs = 
        List.map (fun (op_name, params, body) ->
          (* Create fresh type variables for parameters *)
          let param_types = List.map (fun _ -> next_type_var ()) params in
          
          (* Create a fresh type variable for the result *)
          let result_type = next_type_var () in
          
          (* Add to effect environment *)
          ((op_name, (param_types, result_type)), (params, param_types, body, result_type))
        ) handlers
      in
      
      (* Extract signatures and bodies *)
      let (effect_sigs, handler_bodies) = List.split handler_sigs in
      
      (* Extend effect environment *)
      let extended_eff_env = effect_sigs @ eff_env in
      
      (* Now type check each handler body *)
      List.iter (fun (params, param_types, body, result_type) ->
        (* Create an extended environment with the parameters *)
        let body_env = 
          List.fold_left2 
            (fun env param param_type -> (param, param_type) :: env) 
            env params param_types 
        in
        
        (* Type check the body *)
        match body with
        | ExprBody e -> 
            let (body_type, _) = type_check_expr e body_env extended_eff_env Pure in
            unify body_type result_type
        | BlockBody b ->
            let (body_type, _) = type_check_block b body_env extended_eff_env Pure (Some result_type) in
            unify body_type result_type
      ) handler_bodies;
      
      (* Create a handler type that encompasses all the operations *)
      let handler_effect_in = 
        List.fold_left 
          (fun acc (op_name, (param_types, _)) -> 
            Effect (op_name, param_types, acc))
          Pure effect_sigs
      in
      
      (* The handler transforms the input effect to the pure effect *)
      let handler_type = TyHandler (handler_effect_in, Pure) in
      
      (TyVoid, (name, handler_type) :: env, expected_eff)

(* Type check a program *)
let type_check_program p : unit =
  match p with 
  | Program stmts ->
    (* Initialize the type environment and effect environment *)
    (* Start with an empty environment *)
    let env = [] in
    let eff_env = [] in
    
    (* Type check each statement *)
    ignore (
      List.fold_left (fun (env, current_eff) stmt ->
        let (_, new_env, new_eff) = type_check_stmt stmt env eff_env current_eff in
        (new_env, new_eff)
      ) (env, Pure) stmts
    )
