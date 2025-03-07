(* Use Parse from lib*)
open Source1star

let () =
  let filename = Sys.argv.(1) in
  let ast = Parse.parse_source_file filename in
  print_endline @@ Pp.pretty_print_program ast
