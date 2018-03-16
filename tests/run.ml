
(* let prog_dir = Filename.dirname Sys.argv(0) *)

let create_random_script depth =
  let (filename, out_channel) = Filename.open_temp_file "morsmall_test_" ".sh" in
  let formatter = Format.formatter_of_out_channel out_channel in
  Format.printf "ready to generate...@.";
  Morsmall__Generator.(g_command { depth })
  |> (fun command ->
    Format.printf "Generated.\nready to print...@.";
    Morsmall.pp_print_safe formatter command);
  Format.printf "done!@.";
  Format.pp_print_flush formatter ();
  close_out out_channel;
  filename

let () =
  for i = 1 to 10 do
    Format.printf "==========[ Test n°%d ]==========@." i;

    (* Create a temporary file *)
    let (filename, out_channel) = Filename.open_temp_file "morsmall_test_" ".sh" in
    let formatter = Format.formatter_of_out_channel out_channel in

    (* Create a random script, put it in the file *)
    let ast = Morsmall__Generator.(g_command { depth = 10 }) in
    Morsmall.pp_print_safe formatter ast;
    
    (* Close the file *)
    Format.fprintf formatter "@.";
    close_out out_channel;

    (* Print the AST *)
    Morsmall.pp_print_safe Format.std_formatter ast;
    Format.fprintf Format.std_formatter "@.";
    
    (* Let Morbig and Morsmall parse the file *)
    try
      let csts = Libmorbig.API.parse_file filename in
      let asts = List.map Morsmall.cst_to_ast csts in

      (* Compare *)
      match asts with
      | [Some ast'] ->
         if ast = ast' then
           Format.printf "The two ASTs match!@."
         else
           (
             Format.printf "Uh oh... :-(@.";
             
             Format.fprintf Format.std_formatter "@[<h 2>AST:@\n";
             Morsmall.pp_print_debug Format.std_formatter ast;
             Format.fprintf Format.std_formatter "@]@.";
             
             Format.fprintf Format.std_formatter "@[<h 2>AST':@\n";
             Morsmall.pp_print_debug Format.std_formatter ast';
             Format.fprintf Format.std_formatter "@]@.";
           )
      | _ ->
         Format.printf "Uh oh...@."
    with
    | _ ->
       Format.printf "Morbig could not parse the file! (or an other error... FIXME)@."
  done
