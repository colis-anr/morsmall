
open Morsmall

let () =
  let file = Sys.argv.(1) in
  let lasts = parse_file file in
  let asts = List.map strip_locations lasts in
  let env =
    List.fold_left
      (fun env ast -> Morsmall.interpret env ast)
      (Env.default)
      asts
  in
  exit (Env.rc_to_int env)
