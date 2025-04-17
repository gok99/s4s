(* Use Parse from lib*)
open S4s

let () =
  let filename = Sys.argv.(1) in
  let ast = Parse.parse_source_file filename in
  print_endline @@ Pp.pretty_print_program ast;
  Printf.printf "\n\nInterpreting %s...\n" filename;
  let _ = Ece.eval_program ast in
  (* Printf.printf "Result: %s\n" (Interpreter.string_of_value result) *)
  ()
