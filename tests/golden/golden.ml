(******************************************************************************)
(*                                  Morsmall                                  *)
(*                       A concise AST for POSIX shell                        *)
(*                                                                            *)
(*   Copyright (C) 2017-2023 Yann Régis-Gianas, Ralf Treinen,                 *)
(*   Nicolas Jeannerod                                                        *)
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
(******************************************************************************)

let pf = Format.printf
(* let spf = Format.sprintf *)

module Sys = struct
  include Sys
  (* let commandf format = Format.ksprintf Sys.command format *)
  let ignore_commandf format =
    Format.ksprintf (fun string -> ignore (Sys.command string)) format
end

let bat_or_cat path =
  Sys.ignore_commandf
    {|
      if command -v bat >/dev/null; then
        bat --force-colorization --style numbers %s
      else
        cat %s
      fi
    |}
    (Filename.quote path) (Filename.quote path)

let print_test_info ~name ~from_morbig =
  pf "Test is:@\n@\n  %s `%s`@\n@."
    (if from_morbig then "Morbig's" else "")
    name

let skip_if_no_input path =
  if not (Sys.file_exists (Filename.concat path "input.sh")) then
    (
      (* FIXME: these tests should not be skipped but fixed or deleted. *)
      pf "Test does not have an `input.sh`. Skipping.@.";
      Alcotest.skip ()
    )

let print_input path =
  pf "Input is:@\n@.";
  bat_or_cat (Filename.concat path "input.sh");
  pf "@."

let with_formatter_to_file file f =
  let ochan = open_out file in
  let fmt = Format.formatter_of_out_channel ochan in
  f fmt;
  Format.pp_print_flush fmt ();
  close_out ochan

let print_input_ast path ast =
  with_formatter_to_file (Filename.concat path "input-ast.txt")
    (fun fmt -> Morsmall.pp_print_debug_noloc fmt ast);
  pf "Parsed AST is:@\n@.";
  bat_or_cat (Filename.concat path "input-ast.txt");
  pf "@."

let print_output path =
  pf "Output file is:@\n@.";
  bat_or_cat (Filename.concat path "output.sh");
  pf "@."

let print_output_ast path ast =
  with_formatter_to_file (Filename.concat path "output-ast.txt")
    (fun fmt -> Morsmall.pp_print_debug_noloc fmt ast);
  pf "Parsed AST is:@\n@.";
  bat_or_cat (Filename.concat path "output-ast.txt");
  pf "@."

let check_test_case ~from_morbig name path = fun () ->
  print_test_info ~name ~from_morbig;
  skip_if_no_input path;
  print_input path;
  let ast = Morsmall.parse_file (Filename.concat path "input.sh") in
  print_input_ast path ast;
  with_formatter_to_file (Filename.concat path "output.sh")
    (fun fmt -> Morsmall.pp_print_safe fmt ast);
  print_output path;
  let ast2 = Morsmall.parse_file (Filename.concat path "output.sh") in
  print_output_ast path ast2;
  if not (Morsmall.equal_program_noloc ast ast2) then
    (
      Alcotest.fail "The outputs are not equal"
    )

let rec collect_test_paths name dir =
  List.flatten
    (
      List.map
        (
          fun file ->
            let name = Filename.concat name file in
            let file = Filename.concat dir file in
            if Filename.check_suffix file ".t" then
              [name, file]
            else if Sys.is_directory file then
              collect_test_paths name file
            else
              []
        )
        (Array.to_list (Sys.readdir dir))
    )

let morbig_test_cases =
  List.map
    (fun (name, path) ->
       Alcotest.(test_case name `Quick (check_test_case name path ~from_morbig:true))
    )
    (collect_test_paths "" "morbig/tests/golden/good")

let () =
  Alcotest.run
    ~argv:[|"unused"|] (* FIXME: quick hack; cf top of this file *)
    "golden"
    [
      ("morbig", morbig_test_cases);
    ]
