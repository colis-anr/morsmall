let program_to_string_debug program =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  Morsmall.pp_print_debug_noloc fmt program;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let parse_file fname =
  try Ok (Morsmall.parse_file fname)
  with exn -> Error exn

let print_to_temp_file program =
  let (fname, ochan) = Filename.open_temp_file "morsmall-test-input" ".sh" in
  let fmt = Format.formatter_of_out_channel ochan in
  try
    Morsmall.pp_print_safe fmt program;
    Format.pp_print_flush fmt ();
    Stdlib.close_out ochan;
    Ok fname
  with
    exn ->
    Format.pp_print_flush fmt ();
    Stdlib.close_out ochan;
    Error exn

open QCheck2

let print =
  Test.make
    ~name:"print"
    ~count:200
    ~print:program_to_string_debug
    (Generator.gen_program 1)
  @@
  fun input ->
  Result.is_ok (print_to_temp_file input)

let print_parse =
  Test.make
    ~name:"print and parse"
    ~count:200
    ~print:program_to_string_debug
    (Generator.gen_program 1)
  @@
  fun input ->
  let printing_result = print_to_temp_file input in
  Result.is_ok printing_result
  ==> Result.is_ok (parse_file (Result.get_ok printing_result))

let print_parse_equal =
  Test.make
    ~name:"print and parse; stay equal"
    ~count:200
    ~print:program_to_string_debug
    (Generator.gen_program 1)
  @@
  fun input ->
  let printing_result = print_to_temp_file input in
  Result.is_ok printing_result
  ==> (
    let parsing_result = parse_file (Result.get_ok printing_result) in
    Result.is_ok parsing_result
    ==> (input = Result.get_ok parsing_result)
  )

let () = QCheck_runner.run_tests_main [
    print ;
    print_parse ;
    print_parse_equal ;
  ]
