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

type lexing_position = [%import: Location.lexing_position]
and position = [%import: Location.position]
and 'a located = [%import: 'a Location.located]

[@@deriving show]

type name = [%import: AST.name]
and character_range = [%import: AST.character_range]
and attribute = [%import: AST.attribute]
and word_component = [%import: AST.word_component]
and word = [%import: AST.word]
and pattern = [%import: AST.pattern]
and assignment = [%import: AST.assignment]
and descr = [%import: AST.descr]
and program = [%import: AST.program]
and command = [%import: AST.command]
and case_item = [%import: AST.case_item]
and kind = [%import: AST.kind]

and word' = [%import: AST.word']
and pattern' = [%import: AST.pattern']
and assignment' = [%import: AST.assignment']
and command' = [%import: AST.command']
and case_item' = [%import: AST.case_item']

[@@deriving show]
