(* interpreter.ml - Explicit-Control Evaluator for Source §4 + Delimited Control *)
open Ast
open Machine_t

let use_deep_handler = true
let use_shift_reset = true

type configuration = {
  c: control;
  s: stash;
  e: environment;
  hid: int64;
}

(* Exception for runtime errors *)
exception RuntimeError of string * value

(* Utility functions *)
let is_closure = function
  | Closure _ -> true
  | _ -> false

let is_builtin = function
  | Builtin _ -> true
  | _ -> false

let is_continuation = function
  | Continuation _ -> true
  | _ -> false

let is_unassigned = function
  | UnassignedValue -> true
  | _ -> false

(* Binary operators *)
let apply_binop (op: builtin) v1 v2 = 
  match op, v1, v2 with
  | Plus, Number n1, Number n2 -> Number (n1 +. n2)
  | Plus, String s1, String s2 -> String (s1 ^ s2)
  | Times, Number n1, Number n2 -> Number (n1 *. n2)
  | Minus, Number n1, Number n2 -> Number (n1 -. n2)
  | Divide, Number n1, Number n2 -> Number (n1 /. n2)
  | Modulo, Number n1, Number n2 -> Number (mod_float n1 n2)
  | Less, Number n1, Number n2 -> Boolean (n1 < n2)
  | LessEqual, Number n1, Number n2 -> Boolean (n1 <= n2)
  | GreaterEqual, Number n1, Number n2 -> Boolean (n1 >= n2)
  | Greater, Number n1, Number n2 -> Boolean (n1 > n2)
  | TripleEqual, v1, v2 -> Boolean (v1 = v2)
  | NotTripleEqual, v1, v2 -> Boolean (v1 <> v2)
  | And, Boolean b1, Boolean b2 -> Boolean (b1 && b2)
  | Or, Boolean b1, Boolean b2 -> Boolean (b1 || b2)
  | _ -> raise (RuntimeError ("unsupported binary operation: " ^ (string_of_builtin op), Undefined))

(* Unary operators *)
let apply_unop op v = 
  match op, v with
  | Negate, Number n -> Number (-.n)
  | Not, Boolean b -> Boolean (not b)
  | Not, _ -> raise (RuntimeError ("! expects boolean", v))
  | _ -> raise (RuntimeError ("unsupported unary operation: " ^ (string_of_builtin op), v))

(* Lookup *)
let rec lookup_env name (e: environment) =
  match e with
  | [] -> raise (RuntimeError ("unbound variable: " ^ name, Undefined))
  | frame :: rest ->
      match Hashtbl.find_opt !frame name with
      | Some v -> v
      | None -> lookup_env name rest

let rec assign_env name value e =
  match e with
  | [] -> raise (RuntimeError ("unbound variable: " ^ name, Undefined))
  | frame :: rest ->
      if Hashtbl.mem !frame name then
        Hashtbl.replace !frame name value
      else
        assign_env name value rest

let extend names values env =
  if List.length names <> List.length values then
    raise (RuntimeError ("parameter mismatch", Undefined))
  else
    let new_frame = ref (Hashtbl.create 10) in
    List.iter2 (fun name value -> Hashtbl.add !new_frame name value) names values;
    new_frame :: env

let take_args n lst =
  let rec aux acc n lst =
    if n <= 0 || lst = [] then (acc, lst)
    else aux (List.hd lst :: acc) (n - 1) (List.tl lst)
  in
  aux [] n lst

(* Handler lookup *)
let rec lookup_handler effect_name (handlers: handler list) =
  match handlers with
  | [] -> None
  | h :: rest ->
      match List.assoc_opt effect_name h with
      | Some handler_fn -> Some (handler_fn, h, rest)
      | None -> lookup_handler effect_name rest

(* Scan for declarations *)
let rec scan_declarations = function
  | BlockStatement (Block stmts) ->
      List.concat (List.map scan_declarations stmts)
  | LetDeclaration (name, _) | ConstDeclaration (name, _) | FunctionDeclaration (name, _, _) | HandlerDeclaration (name, _) ->
      [name]
  | _ -> []

(* Global environment setup *)
let setup_global_environment () = 
  let constants = [
    ("undefined", Undefined);
    ("null", Null);
    ("true", Boolean true);
    ("false", Boolean false);
    ("math_PI", Number 3.14159265358979323846);
    (* Add other mathematical constants here *)
  ] in
  let builtins = [
    ("display", Builtin Display);
    ("stringify", Builtin Stringify);
    ("math_sqrt", Builtin Math_Sqrt);
    ("math_round", Builtin Math_Round);
    ("is_number", Builtin Is_Number);
    ("is_string", Builtin Is_String);
    ("is_function", Builtin Is_Function);
    ("is_boolean", Builtin Is_Boolean);
    ("is_undefined", Builtin Is_Undefined);
    ("pair", Builtin Pair);
    ("head", Builtin Head);
    ("tail", Builtin Tail);
    ("is_null", Builtin Is_Null);
    ("is_pair", Builtin Is_Pair);
    ("list", Builtin List);
    ("shift", Builtin Shift);
    ("reset", Builtin Reset);
  ] in
  let global_frame = ref (Hashtbl.create 20) in
  List.iter (fun (name, value) -> Hashtbl.add !global_frame name value) constants;
  List.iter (fun (name, value) -> Hashtbl.add !global_frame name value) builtins;
  [global_frame]

let pop_until_reset_control_marker config =
  let rec aux acc c = 
    match c with
    | [] -> (List.rev acc, [])
    | ResetControlMarker :: _ -> 
      let k_c = if use_shift_reset
        then List.rev (ResetControlMarker :: acc)
        else List.rev acc
      in
      (k_c, c)
    | x :: xs -> aux (x :: acc) xs
  in
  let (k_c, new_c) = aux [] config.c in
  (k_c, { config with c = new_c })

let pop_until_reset_stash_marker config =
  let rec aux acc s = 
    match s with
    | [] -> (List.rev acc, [])
    | ResetStashMarker :: _ -> 
      let k_s = if use_shift_reset
        then List.rev (ResetStashMarker :: acc)
        else List.rev acc
      in
      (k_s, s)
    | x :: xs -> aux (x :: acc) xs
  in
  let (k_s, new_s) = aux [] config.s in
  (k_s, { config with s = new_s })

let pop_until_reset_handler_control_marker op control =
  let rec aux acc c = 
    match c with
    | [] -> raise (RuntimeError ("unbound effect: " ^ op, Undefined))
    | ResetHandlerControlMarker (handler, id) :: xs ->
        begin match List.assoc_opt op handler with
        | Some handler_fn -> 
            let k_c = if use_deep_handler
              then List.rev (ResetHandlerControlMarker (handler, id) :: acc)
              else List.rev acc
            in
            (handler_fn, id, k_c, xs)
        | None -> aux (ResetHandlerControlMarker (handler, id) :: acc) xs
        end
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] control

let pop_until_reset_handler_stash_marker id stash =
  let rec aux acc s =
    match s with
    | [] -> raise (RuntimeError ("unbound effect", Undefined))
    | ResetHandlerStashMarker id' :: xs when id' = id ->
        let k_s = if use_shift_reset
          then List.rev (ResetHandlerStashMarker id' :: acc)
          else List.rev acc
        in
        (k_s, xs)
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] stash

let microcode_expression expr config =
  match expr with
  | NumberLiteral n -> { config with s = Number n :: config.s }
  | BooleanLiteral b -> { config with s = Boolean b :: config.s }
  | StringLiteral s -> { config with s = String s :: config.s }
  | NameExpression name -> 
      let v = lookup_env name config.e in
      { config with s = v :: config.s }
  | BinaryExpression (op, e1, e2) ->
      { config with
        c = (Expression e1) ::
            (Expression e2) ::
            (BinopInstr (buitin_of_binary_operator op)) :: config.c
      }
  | UnaryExpression (op, e) ->
      { config with
        c = (Expression e) ::
            (UnopInstr (buitin_of_unary_operator op)) :: config.c
      }
  | LogicalExpression (op, e1, e2) ->
      { config with
        c = (Expression e1) ::
            (Expression e2) ::
            (BinopInstr (buitin_of_logical_operator op)) :: config.c }
  | FunctionApplication (func, args) ->
      { config with
        c = (Expression func) ::
            (List.map (fun arg -> Expression arg) args) @
            (AppInstr (List.length args) :: config.c) }
  | LambdaExpression (params, body) ->
      let closure = Closure { params; body; env = config.e } in
      { config with s = closure :: config.s }
  | ConditionalExpression (cond, then_expr, else_expr) ->
      { config with
        c = (Expression cond) ::
            (BranchInstr { 
              cons = ExpressionStatement then_expr; 
              alt = ExpressionStatement else_expr }) :: config.c }
  | PerformExpression (op, args) ->
      let args = List.map (fun arg -> Expression arg) args
      |> fun a -> PerformInstr (op, List.length args) :: a
      |> List.rev
      in
      { config with
        c = args @ config.c;
      }
  | WithHandler (expr, block) ->
    {
      config with
      c = (Expression expr) :: 
          (RunWithHandlerInstr block) :: config.c;
    }

let microcode_statement stmt config =
  match stmt with
  | ConstDeclaration (name, expr) | LetDeclaration (name, expr) ->
      { config with
        c = (Expression expr) ::
            (AssignmentInstr name) ::
            PopInstr ::
            (LitInstr Undefined) :: config.c }
  | FunctionDeclaration (name, params, body) ->
    let lambda = LambdaExpression (params, BlockBody body) in
    let const = ConstDeclaration (name, lambda) in
    { config with
      c = (Statement const) :: config.c; }
  | ReturnStatement expr ->
      { config with
        c = Expression expr :: ResetInstr :: config.c }
  | IfStatement (cond, then_block, else_block) ->
      { config with
        c = (Expression cond) ::
            (BranchInstr { 
              cons = BlockStatement then_block; 
              alt = BlockStatement else_block }) :: config.c }
  | IfElseIfStatement (cond, then_block, else_stmt) ->
      { config with
        c = (Expression cond) ::
            (BranchInstr { 
              cons = BlockStatement then_block; 
              alt = else_stmt  }) :: config.c }
  | WhileStatement (cond, body) ->
      { config with
        c = (Expression cond) ::
            (WhileInstr { 
              pred = cond; 
              body = BlockStatement body }) ::
            (LitInstr Undefined) :: config.c }
  | BlockStatement (Block stmts) ->
      (* Extend environment *)
      let locals = scan_declarations (BlockStatement (Block stmts)) in
      let unassigned = List.map (fun _ -> UnassignedValue) locals in
      let new_env = extend locals unassigned config.e in
      let new_cmds = (List.map (fun stmt -> PopInstr :: [Statement stmt]) stmts)
        |> List.flatten
        |> List.tl
      in
      { config with
        c = new_cmds @ config.c;
        e = new_env }
  | ExpressionStatement expr ->
      { config with
        c = (Expression expr) :: config.c }
  | BreakStatement | ContinueStatement ->
      raise (RuntimeError ("unsupported statement", Undefined))
  | AssignmentStatement (name, expr) ->
      { config with
        c = (Expression expr) ::
            (AssignmentInstr name) :: config.c }
  | HandlerDeclaration (name, ops) -> 
      let handler = List.map (fun (op_name, params, body) ->
        let closure = Closure { params; body; env = config.e } in
        (op_name, closure)) ops 
      in
      {
        config with
        s = Handler handler :: config.s;
        c = (AssignmentInstr name) ::
            PopInstr ::
            (LitInstr Undefined) :: config.c
      }

let rec value_of_list xs =
  match xs with
  | [] -> Null
  | x :: xs' -> Pair { fst = x; snd = value_of_list xs' }

let microcode_builtin_app bi args new_s config =
  let ret_v = fun v -> { config with s = v :: new_s } in
  match bi with
  | Display -> 
      let v = List.hd args in
      print_endline (string_of_value v);
      ret_v v
  | Stringify ->
      let v = List.hd args in
      ret_v (String (string_of_value v))
  | Math_Sqrt ->
      let v = List.hd args in
      ret_v (match v with
        | Number n -> Number (sqrt n)
        | _ -> raise (RuntimeError ("sqrt expects a number", v)))
  | Math_Round ->
      let v = List.hd args in
      ret_v (match v with
        | Number n -> Number (Float.round n)
        | _ -> raise (RuntimeError ("round expects a number", v)))
  | Is_Number ->
      let v = List.hd args in
      ret_v (Boolean (match v with Number _ -> true | _ -> false))
  | Is_String ->
      let v = List.hd args in
      ret_v (Boolean (match v with String _ -> true | _ -> false))
  | Is_Function ->
      let v = List.hd args in
      ret_v (Boolean (match v with Closure _ -> true | Builtin _ -> true | _ -> false))
  | Is_Boolean ->
      let v = List.hd args in
      ret_v (Boolean (match v with Boolean _ -> true | _ -> false))
  | Is_Undefined ->
      let v = List.hd args in
      ret_v (Boolean (match v with Undefined -> true | _ -> false))
  | Pair ->
      let v1 = List.hd args in
      let v2 = List.hd (List.tl args) in
      ret_v (Pair { fst = v1; snd = v2 })
  | Head ->
      let v = List.hd args in
      ret_v (match v with
       | Pair { fst; _ } -> fst
       | _ -> raise (RuntimeError ("head expects a pair", v)))
  | Tail ->
      let v = List.hd args in
      ret_v (match v with
       | Pair { snd; _ } -> snd
       | _ -> raise (RuntimeError ("tail expects a pair", v)))
  | Is_Null ->
      let v = List.hd args in
      ret_v (Boolean (match v with Null -> true | _ -> false))
  | Is_Pair ->
      let v = List.hd args in
      ret_v (Boolean (match v with Pair _ -> true | _ -> false))
  | List -> value_of_list args |> ret_v
  | Reset ->
    {
      config with
      c = (AppInstr 0) :: ResetControlMarker :: config.c;
      s = List.hd args :: ResetStashMarker :: new_s;
    }
  | Shift ->
    let (k_c, new_config) = pop_until_reset_control_marker { config with s = new_s } in
    let (k_s, new_config) = pop_until_reset_stash_marker new_config in
    {
      new_config with
      c = (AppInstr 1) :: new_config.c;
      s = (Continuation { stash = k_s; control = k_c; env = new_config.e }) :: 
          List.hd args :: new_config.s;
    }
  | _ -> raise (RuntimeError ("unsupported builtin: " ^ (string_of_builtin bi), Undefined))

let microcode_app arity config =
  let args, rest = take_args arity config.s in
  let func = List.hd rest in
  let new_s = List.tl rest in
  match func with
  | Closure { params; body; env} ->
    (* extend closure env *)
    let new_env = extend params args env in
    let body =
      match body with
      | BlockBody blk -> Statement (BlockStatement blk)
      | ExprBody expr -> Expression expr
    in
    if (config.c = [] || is_env_i (List.hd config.c))
    (* current E not needed  *)
    then {
      config with
      c = body :: MarkInstr :: config.c;
      s = new_s;
      e = new_env;
    }
    else if (is_reset_i (List.hd config.c))
    (* Tail call *)
    then {
      config with
      c = body :: List.tl config.c;
      s = new_s;
      e = new_env;
    }
    else
    (* General case *)
    { 
      config with
      c = body :: MarkInstr :: (EnvInstr config.e) :: config.c;
      s = new_s;
      e = new_env;
    }
  | Continuation { control; stash; env } ->
    {
      config with
      c = control @ ((EnvInstr config.e) :: config.c);
      s = List.hd args :: stash @ new_s;
      e = env;
    }
  | Builtin b -> microcode_builtin_app b args new_s config
  | _ -> raise (RuntimeError ("", Undefined))

let microcode cmd config =
  match cmd with
  | Expression expr -> microcode_expression expr config
  | Statement stmt -> microcode_statement stmt config
  | PopInstr -> { config with s = List.tl config.s }
  | LitInstr v -> { config with s = v :: config.s }
  | UnopInstr op -> 
      let v = List.hd config.s in
      let result = apply_unop op v in
      { config with s = result :: (List.tl config.s) }
  | BinopInstr op -> 
      let v2 = List.hd config.s in
      let v1 = List.nth config.s 1 in
      let result = apply_binop op v1 v2 in
      { config with s = result :: (List.tl (List.tl config.s)) }
  | AppInstr arity -> microcode_app arity config
  | AssignmentInstr name ->
      let v = List.hd config.s in
      assign_env name v config.e;
      config
  | BranchInstr { cons; alt } ->
      let v = List.hd config.s in
      let new_c = if v = Boolean true then cons else alt in
      { config with 
        c = Statement new_c :: config.c;
        s = List.tl config.s }
  | WhileInstr { pred; body } ->
      let v = List.hd config.s in
      let new_c = if v = Boolean true 
        then Statement body :: PopInstr :: (Expression pred) :: cmd :: config.c
        else config.c
      in
      { config with 
        c = new_c;
        s = List.tl config.s 
      }
  | EnvInstr env ->
    {
      config with
      e = env
    }
  | ResetInstr ->
    let hd = List.hd config.c in
    let rest = List.tl config.c in
    {
      config with
      c = if is_mark_i hd  
        then rest
        else ResetInstr :: rest
    }
  | MarkInstr -> config
  | ResetControlMarker ->
    let ret = List.hd config.s in
    let m = List.hd (List.tl config.s) in
    let rest = List.tl (List.tl config.s) in
    begin match m with
    | ResetStashMarker ->
      {
        config with
        s = ret :: rest
      }
    | v -> raise (RuntimeError ("encountered unexpected stash value at reset control marker", v))
    end
  | RunWithHandlerInstr block ->
    let v = List.hd config.s in
    let handler = match v with
    | Handler h -> h
    | _ -> raise (RuntimeError ("expected handler", v))
    in
    {
      config with
      s = ResetHandlerStashMarker config.hid :: List.tl config.s;
      c = (Statement (BlockStatement block)) :: ResetHandlerControlMarker (handler, config.hid) :: config.c;
      hid = Int64.add config.hid 1L;
    }
  | PerformInstr (op, arity) ->
      let args, stash = take_args arity config.s in
      let (handler_fn, id, k_c, new_c) = pop_until_reset_handler_control_marker op config.c in
      let (k_s, new_s) = pop_until_reset_handler_stash_marker id stash in
      let k = Continuation { stash = k_s; control = k_c; env = config.e } in
      {
        config with
        s = args @ (k :: handler_fn :: new_s);
        c = (AppInstr (List.length args + 1)) :: new_c;
      }
  | ResetHandlerControlMarker (_, id) ->
    let ret = List.hd config.s in
    let m = List.hd (List.tl config.s) in
    let rest = List.tl (List.tl config.s) in
    begin match m with
    | ResetHandlerStashMarker id' when id = id' ->
      {
        config with
        s = ret :: rest
      }
    | v -> raise (RuntimeError ("encountered unexpected stash value at reset handler control marker", v)) 
    end

let print_control control =
  Printf.printf "Control: ";
  List.iter (fun cmd -> Printf.printf "(%s) " (string_of_command cmd)) control;
  Printf.printf "\n"

let print_stash stash =
  Printf.printf "Stash: ";
  List.iter (fun v -> Printf.printf "%s " (string_of_value v)) stash;
  Printf.printf "\n"

let print_env env =
  Printf.printf "Environment:\n";
  List.iteri (fun i frame ->
    Printf.printf "Frame %d:\n" i;
    Hashtbl.iter (fun k v -> Printf.printf "  %s: %s\n" k (string_of_value v)) !frame
  ) env;
  Printf.printf "\n"

let print_handler_stack handlers =
  Printf.printf "Handler Stack:\n";
  List.iteri (fun i handler ->
    Printf.printf "Handler %d:\n" i;
    List.iter (fun (name, _) ->
      Printf.printf "  %s\n" name
    ) handler
  ) handlers;
  Printf.printf "\n"

let eval_program ast = 
  let initial_block = match ast with
  | Program stmts -> Statement (BlockStatement (Block stmts))
  in
  
  let config = ref {
    c = [initial_block];
    s = [];
    e = setup_global_environment ();
    hid = 0L;
  } in

  let rec step count =
    if count <= 0 then
      raise (RuntimeError ("maximum step count exceeded", Undefined))
    else
      match !config.c with
      | [] -> List.hd !config.s
      | _ ->
        (* Execute the next command *)
        let current_cmd = List.hd !config.c in
        (* print_control !config.c;
        Printf.printf "Executing: %s\n" (string_of_command current_cmd); *)
        config := {
          !config with
          c = List.tl !config.c;
        };
        config := microcode current_cmd !config;
        (* print_stash !config.s; *)
        (* print_handler_stack !config.h; *)
        (* print_env !config.e; *)
        (* Printf.printf "==========================\n"; *)
        step (count - 1)
    in
    step 10000000
