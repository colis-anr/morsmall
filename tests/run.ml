(******************************************************************************)
(*                                                                            *)
(*                                  Morsmall                                  *)
(*                       A concise AST for POSIX shell                        *)
(*                                                                            *)
(*   Copyright (C) 2017  Yann Régis-Gianas, Ralf Treinen, Nicolas Jeannerod   *)
(*                                                                            *)
(*   This program is free software: you can redistribute it and/or modify     *)
(*   it under the terms of the GNU General Public License as published by     *)
(*   the Free Software Foundation, either version 3 of the License, or        *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU General Public License for more details.                             *)
(*                                                                            *)
(*   You should have received a copy of the GNU General Public License        *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                            *)
(******************************************************************************)

let fpf = Format.fprintf

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

let with_formatter_to_string f =
  let buf = Buffer.create 8 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let pp_print_input_ast fmt input =
  Morsmall.pp_print_debug_noloc fmt input

let pp_print_concrete fmt input =
  Morsmall.pp_print_safe fmt input

let pp_print_parsed_ast fmt input =
  let fname = Result.get_ok (print_to_temp_file input) in
  let output = Result.get_ok (parse_file fname) in
  Morsmall.pp_print_debug_noloc fmt output

open QCheck2

let print =
  Test.make
    ~name:"print"
    ~count:20000
    ~print:(fun program ->
        with_formatter_to_string @@ fun fmt ->
        fpf fmt "Input AST:@\n@\n@[<2>  %a@]@\n"
          pp_print_input_ast program)
    (Generator.gen_program 1)
  @@
  fun input ->
  Result.is_ok (print_to_temp_file input)

let print_parse =
  Test.make
    ~name:"print and parse"
    ~count:20000
    ~print:(
      fun program ->
        with_formatter_to_string @@ fun fmt ->
        fpf fmt "Input AST:@\n@\n@[<2>  %a@]@\n@\nAs a Shell script:@\n@\n@[<2>  %a@]@\n"
          pp_print_input_ast program
          pp_print_concrete program
    )
    (Generator.gen_program 1)
  @@
  fun input ->
  let printing_result = print_to_temp_file input in
  Result.is_ok printing_result
  ==> Result.is_ok (parse_file (Result.get_ok printing_result))

let print_parse_equal =
  Test.make
    ~name:"print and parse; stay equal"
    ~count:20000
    ~print:(
      fun program ->
        with_formatter_to_string @@ fun fmt ->
        fpf fmt "Input AST:@\n@\n@[<2>  %a@]@\n@\nAs a Shell script:@\n@\n@[<2>  %a@]@\n@\nParsed AST:@\n@\n@[<2>  %a@]@\n"
          pp_print_input_ast program
          pp_print_concrete program
          pp_print_parsed_ast program
    )
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
