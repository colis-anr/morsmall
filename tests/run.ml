
exception CouldntParse of Morsmall.AST.program
exception ASTsDontMatch of Morsmall.AST.program * Morsmall.AST.program

let generator_parameters = Generator.default_parameters

let number_of_tests = 100

(* *)

let run_one_test () =
  (* Create a temporary file *)
  let (filename, out_channel) = Filename.open_temp_file "morsmall_test_" ".sh" in
  let formatter = Format.formatter_of_out_channel out_channel in

  (* Create a random script, put it in the file *)
  let ast = Generator.(g_program generator_parameters) in
  Morsmall.pp_print_safe formatter ast;

  (* Close the file *)
  Format.fprintf formatter "@.";
  close_out out_channel;

  try
    (
      (* Parse the file with Morbig and Morsmall *)
      let ast' =
        try
          Morsmall.parse_file filename
        with
          Morsmall.SyntaxError _pos ->
          raise (CouldntParse ast)
      in

      (* Compare *)
      if not (Morsmall.AST.equal_program ast ast') then
           raise (ASTsDontMatch (ast, ast'))
    );

    (* Clean the temporary file *)
    Sys.remove filename
  with
  | _ as exn ->
     Sys.remove filename;
     raise exn

let () =
  let errors = ref 0 in

  for i = 1 to number_of_tests do
    Format.printf "Running test #%d...\r@?" i;
    try
      run_one_test ()
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
             | CouldntParse ast ->
                Format.fprintf formatter "\nMorbig could not parse the file produced by Morsmall's printer.\n";
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
