let pf = Format.printf
let fpf = Format.fprintf
let spf = Format.sprintf

(******************************************************************************)
(* Logging                                                                    *)

let () = Logs.set_level ~all:true (Some Logs.Info)

let level_to_string = function
  | Logs.Debug -> "DBG"
  | Logs.Info -> "INF"
  | Logs.Warning -> "WRN"
  | Logs.Error -> "ERR"
  | Logs.App -> "APP"

let level_to_color = function
  | Logs.Debug -> "\027[37m" (* gray *)
  | Logs.Info -> ""               (* white *)
  | Logs.Warning -> "\027[33m"    (* yellow *)
  | Logs.Error -> "\027[31m"      (* red *)
  | Logs.App -> "\027[1m"         (* white bold *)

let () =
  Logs.set_reporter
    (let report _src level ~over k msgf =
       msgf @@ fun ?header:_ ?tags:_ fmt ->
       Format.kfprintf
         (fun _ -> over (); k ())
         Format.err_formatter
         ("@[<h 2>%s%.3f | %s | " ^^ fmt ^^ "\027[0m@]@.")
         (level_to_color level)
         (Sys.time ())
         (level_to_string level)
     in
     { Logs.report })

module Log = (val Logs.(src_log default) : Logs.LOG)

(******************************************************************************)

exception CouldntParse of Morsmall.AST.program
exception ASTsDontMatch of Morsmall.AST.program * Morsmall.AST.program

let generator_parameters = Generator.default_parameters

let number_of_tests = 100

(******************************************************************************)
(* Artifacts                                                                  *)

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

(******************************************************************************)
(* Test runner                                                                *)

let run_one_test ~test_number =
  create_artifacts_directory ~test_number;

  Log.debug (fun m -> m "Generating AST...");
  let input = Generator.(g_nonempty_program generator_parameters) in

  let fname = artifacts_file ~test_number "input.show" in
  Log.debug (fun m -> m "Printing it to `%s`..." fname);
  with_file fname (fun fmt -> Morsmall.pp_print_debug fmt input);

  let fname = artifacts_file ~test_number "input.json" in
  Log.debug (fun m -> m "Printing it to `%s`..." fname);
  with_file fname (fun fmt -> Morsmall.pp_print_json fmt input);

  let fname = artifacts_file ~test_number "input.sh" in
  Log.debug (fun m -> m "Printing it to `%s`..." fname);
  with_file fname (fun fmt -> Morsmall.pp_print_safe fmt input);

  Log.debug (fun m -> m "Calling Morbig and Morsmall on it...");
  let output =
    try Morsmall.parse_file fname
    with Morsmall.SyntaxError _pos -> raise (CouldntParse input)
  in

  let fname = artifacts_file ~test_number "output.show" in
  Log.debug (fun m -> m "Printing it to `%s`..." fname);
  with_file fname (fun fmt -> Morsmall.pp_print_debug fmt output);

  let fname = artifacts_file ~test_number "output.json" in
  Log.debug (fun m -> m "Printing it to `%s`..." fname);
  with_file fname (fun fmt -> Morsmall.pp_print_json fmt output);

  if not (Morsmall.AST.equal_program input output) then
    (
      Log.debug (fun m -> m "AST do not match. Incoming error.");
      raise (ASTsDontMatch (input, output))
    )

let () =
  let errors = ref 0 in
  for test_number = 1 to number_of_tests do
    Log.info (fun m -> m "Running test #%d..." test_number);
    try
      run_one_test ~test_number
    with
      exn ->
      (
        incr errors;
        match exn with
        | CouldntParse _input ->
          Log.warn (fun m -> m "Test #%d failed: Morbig could not parse the file produced by Morsall's printer." test_number)
        | ASTsDontMatch (_input, _output) ->
          Log.warn (fun m -> m "Test #%d failed: Input and output ASTs do not match." test_number)
        | exn ->
          Log.err (fun m -> m "Test #%d failed with unexpected exception." test_number);
          raise exn
      )
  done;

  if !errors = 0 then
    (
      Log.info (fun m -> m "Successfully ran %d tests." number_of_tests);
      exit 0
    )
  else
    (
      Log.err (fun m -> m "While running %d tests, got %d errors. See the reports for more details." number_of_tests !errors);
      exit 1
    )
