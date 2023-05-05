let pf = Format.printf
let fpf = Format.fprintf
let spf = Format.sprintf

exception CouldntParse of Morsmall.AST.program
exception ASTsDontMatch of Morsmall.AST.program * Morsmall.AST.program

let generator_parameters = Generator.default_parameters

let number_of_tests = 100

let artifacts_directory_prefix =
  "artifacts"

let create_artifacts_directory_prefix_if_not_exist () =
  if not Sys.(file_exists artifacts_directory_prefix
              && is_directory artifacts_directory_prefix) then
    Unix.mkdir artifacts_directory_prefix 0o777

let artifacts_directory ~test_number =
  spf "%s/%d" artifacts_directory_prefix test_number

let create_artifacts_directory ~test_number =
  create_artifacts_directory_prefix_if_not_exist ();
  Unix.mkdir (artifacts_directory ~test_number) 0o777

let artifacts_file ~test_number artifact_name =
  Filename.concat (artifacts_directory ~test_number) artifact_name

let with_file fname cont =
  let ochan = open_out fname in
  let fmt = Format.formatter_of_out_channel ochan in
  try
    let res = cont fmt in
    Format.pp_print_flush fmt ();
    Stdlib.close_out ochan;
    res
  with
    exn ->
    Format.pp_print_flush fmt ();
    Stdlib.close_out ochan;
    raise exn

let run_one_test ~test_number =
  create_artifacts_directory ~test_number;

  let input = Generator.(g_program generator_parameters) in

  let fname = artifacts_file ~test_number "input.show" in
  with_file fname (fun fmt -> Morsmall.pp_print_debug fmt input);

  let fname = artifacts_file ~test_number "input.sh" in
  with_file fname (fun fmt -> Morsmall.pp_print_safe fmt input);

  let output =
    try Morsmall.parse_file fname
    with Morsmall.SyntaxError _pos -> raise (CouldntParse input)
  in

  let fname = artifacts_file ~test_number "output.show" in
  with_file fname (fun fmt -> Morsmall.pp_print_debug fmt output);

  if not (Morsmall.AST.equal_program input output) then
    raise (ASTsDontMatch (input, output))

let () =
  pf "@.";
  let errors = ref 0 in
  for test_number = 1 to number_of_tests do
    pf "Running test #%d...\r@?" test_number;
    try
      run_one_test ~test_number
    with
      exn ->
      (
        incr errors;
        pf "Error while running test #%d.@." test_number;
        (
          match exn with
          | CouldntParse _input ->
            pf "Morbig could not parse the file produced by Morsall's printer.@."
          | ASTsDontMatch (_input, _output) ->
            pf "The input and output ASTs do not match.@."
          | exn -> raise exn
        );
        pf "@."
      )
  done;

  if !errors = 0 then
    (
      pf "Successfully ran %d tests.@." number_of_tests;
      exit 0
    )
  else
    (
      pf "While running %d tests, got %d errors. See the reports for more details.@." number_of_tests !errors;
      exit 1
    )
