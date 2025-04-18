(* Use Parse from lib*)
open S4s

let () =
  let filename = Sys.argv.(1) in
  let ast = Parse.parse_source_file filename in
  (* let _ = Typecheck.type_check_program ast in *)
  print_endline @@ Pp.pretty_print_program ast;
  Printf.printf "\n\nInterpreting %s...\n" filename;
  let result = Ece.eval_program ast in
  Printf.printf "Result: %s\n" (Machine_t.string_of_value result)
