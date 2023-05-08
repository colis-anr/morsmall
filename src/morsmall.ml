(***************************************************************************)
(*                                 Morsmall                                *)
(*                      A concise AST for POSIX shell                      *)
(*                                                                         *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Ralf Treinen,          *)
(*  Nicolas Jeannerod                                                      *)
(*                                                                         *)
(*  This program is free software: you can redistribute it and/or modify   *)
(*  it under the terms of the GNU General Public License as published by   *)
(*  the Free Software Foundation, either version 3 of the License, or      *)
(*  (at your option) any later version.                                    *)
(*                                                                         *)
(*  This program is distributed in the hope that it will be useful,        *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          *)
(*  GNU General Public License for more details.                           *)
(*                                                                         *)
(*  You should have received a copy of the GNU General Public License      *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.  *)
(***************************************************************************)

module AST = AST
type t = AST.program

exception SyntaxError of Location.lexing_position

let from_CST = CST_to_AST.program__to__program

let parse_file filename =
  let open Morbig in
  (
    try
      Morbig.parse_file filename
    with
    | Errors.DuringParsing position
    | Errors.DuringLexing (position, _) ->
       raise (SyntaxError position)
  )
  |> from_CST

let pp_print_safe = SafePrinter.pp_program
let pp_print_json = JsonPrinter.pp_program
let pp_print_json_noloc = JsonNonLocatedPrinter.pp_program
let pp_print_debug = DebugPrinter.pp_program

let equal_program = LocatedEquality.equal_program
let equal_program_noloc = NonLocatedEquality.equal_program

include ASTUtils

(* other modules *)

module Location = Location
module CST_to_AST = CST_to_AST
module Utilities = Morsmall_utilities
module Visitors = Visitors

module Printer = struct
  module Safe = SafePrinter
  module Json = JsonPrinter
  module JsonNonLocated = JsonNonLocatedPrinter
  module Debug = DebugPrinter
end

module Equality = struct
  module Located = LocatedEquality
  module NonLocated = NonLocatedEquality
end
