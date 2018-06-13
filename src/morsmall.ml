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

exception SyntaxError of Libmorbig.CST.position * string

let parse_file filename =
  let csts =
    try Libmorbig.API.parse_file filename
    with _ ->
      (* FIXME: when issue #22 in Morbig is fixed, match for Morbig's errors. *)
      raise (SyntaxError (Libmorbig.CSTHelpers.dummy_position, "An error occured"))
  in
  let asts =
    List.map
      Converter.complete_command__to__command_option
      csts
  in
  if asts = [None] then
    []
  else
    List.map
      (function
       | None -> assert false
       | Some ast -> ast)
      asts


module LAST = AST.LAST
module AST = AST.AST

let strip_locations = LocationMapper.command

let pp_print_safe = SafePrinter.pp_command
let pp_print_debug = AST.pp_command
