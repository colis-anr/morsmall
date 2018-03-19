
exception MorbigCouldntParse of Morsmall.AST.command * string
exception ASTsDontMatch of Morsmall.AST.command * Morsmall.AST.command

let run_one_test test_number =
  (* Create a temporary file *)
  let (filename, out_channel) = Filename.open_temp_file "morsmall_test_" ".sh" in
  let formatter = Format.formatter_of_out_channel out_channel in

  (* Create a random script, put it in the file *)
  let ast = Morsmall__Generator.(g_command { depth = 10 }) in
  Morsmall.pp_print_safe formatter ast;

  (* Close the file *)
  Format.fprintf formatter "@.";
  close_out out_channel;

  try
    (
      (* Parse the file with Morbig *)
      let csts =
        try Libmorbig.API.parse_file filename
        with _ -> raise (MorbigCouldntParse (ast, "FIXME: catch error message"))
      in

      (* Import it with Morsmall *)
      let asts = List.map Morsmall.cst_to_ast csts in

      (* Compare *)
      match asts with
      | [Some ast'] ->
         if not (Morsmall.AST.equal_command ast ast') then
           raise (ASTsDontMatch (ast, ast'))
      | _ ->
         failwith (Format.sprintf "Error in test #%d: could not understand Morbig's output.@." test_number)
    );

    (* Clean the temporary file *)
    Sys.remove filename
  with
  | _ as exn ->
     Sys.remove filename;
     raise exn

let number_of_tests = 10

let () =
  let errors = ref 0 in
  
  for i = 1 to number_of_tests do
    Format.printf "Running test #%d...\r@?" i;
    try
      run_one_test i
    with
    | _ as exn ->
       (
         (* Generate a report *)

         let filename = "morsmall_test_report_"^(string_of_int i)^".md" in
         let out_channel = open_out filename in
         let formatter = Format.formatter_of_out_channel out_channel in
         Format.fprintf formatter "# Morsmall Test Engine -- Report on Test #%d\n" i;
         Format.fprintf formatter "\n## The Error\n";
         let ast =
           (
             match exn with
             | MorbigCouldntParse (ast, error) ->
                Format.fprintf formatter "\nMorbig could not parse the file produced by Morsmall's printer.\n";
                Format.fprintf formatter "It failed with the following error message:\n> %s\n" error;
                ast

             | ASTsDontMatch (ast, _) ->
                Format.fprintf formatter "\nThe parsed AST does not coincide with the generated one.\n";
                ast

             | _ as exn -> raise exn
           )
         in
         Format.fprintf formatter "\n## Generated AST\n\n%a\n" Morsmall.pp_print_debug ast;
         Format.fprintf formatter "\n## Generated Shell Script\n\n%a\n" Morsmall.pp_print_safe ast;
         (
           match exn with
           | ASTsDontMatch (_, ast') ->
              Format.fprintf formatter "\n## Parsed AST\n\n%a\n" Morsmall.pp_print_debug ast'

           | _ -> ()
         );
         Format.fprintf formatter "@?";
         close_out out_channel;

         (* Complain *)
         Format.eprintf "Error in test #%d: check report in '%s'@." i filename;
         incr errors
       )
  done;

  if !errors = 0 then
    (
      Format.printf "Successfully ran %d tests." number_of_tests;
      exit 0
    )
  else
    (
      Format.printf "While running %d tests, got %d errors. See the reports for more details.@." number_of_tests !errors;
      exit 1
    )
