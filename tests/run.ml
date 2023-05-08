let program_to_string_debug program =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  Morsmall.pp_print_debug fmt program;
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
    fname
  with
    exn ->
    Format.pp_print_flush fmt ();
    Stdlib.close_out ochan;
    raise exn

let print_and_parse =
  QCheck2.Test.make
    ~name:"printed programs are parseable"
    ~count:200
    ~print:program_to_string_debug
    (Generator.gen_program 1)
  @@
  fun input ->
  let fname = print_to_temp_file input in
  Result.is_ok (parse_file fname)

let print_parse_equal =
  QCheck2.Test.make
    ~name:"printed programs are parseable to the same AST"
    ~count:200
    ~print:program_to_string_debug
    (Generator.gen_program 1)
  @@
  fun input ->
  let fname = print_to_temp_file input in
  match parse_file fname with
  | Error _exn -> false
  | Ok output -> Morsmall.equal_program input output

let () = QCheck_runner.run_tests_main [
    print_and_parse ;
    print_parse_equal ;
  ]
