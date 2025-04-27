(* Use Parse from lib*)
open S4s

let () =
  let use_typed = Sys.argv.(1) = "typed" in
  let filename = Sys.argv.(2) in
  let ast = if use_typed 
  then
    let t_ast = Parse.parse_typed_source_file filename in
    let t,a = Typecheck.type_check t_ast in
    Printf.printf "Result type: %s, %s \n" (Ast_typed.string_of_type t) a;
    Ast_typed.program_of_typed_program t_ast
  else
    Parse.parse_source_file filename 
  in
  (* let _ = Typecheck.type_check_program ast in *)
  print_endline @@ Pp.pretty_print_program ast;
  Printf.printf "\n\nInterpreting %s...\n" filename;
  let result = Ece.eval_program ast in
  Printf.printf "Result: %s\n" (Machine_t.string_of_value result)
