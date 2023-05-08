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

type lexing_position = [%import: Morbig.CST.lexing_position]
and position = [%import: Morbig.CST.position]
and 'a located = [%import: 'a Morbig.CST.located]

[@@deriving eq, show {with_path=false}, yojson]

class virtual ['a] located_iter      = ['a] Morbig.CSTVisitors.located_iter
class virtual ['a] located_map       = ['a] Morbig.CSTVisitors.located_map
class virtual ['a] located_reduce    = ['a] Morbig.CSTVisitors.located_reduce
class virtual ['a] located_mapreduce = ['a] Morbig.CSTVisitors.located_mapreduce
class virtual ['a] located_iter2     = ['a] Morbig.CSTVisitors.located_iter2
class virtual ['a] located_map2      = ['a] Morbig.CSTVisitors.located_map2
class virtual ['a] located_reduce2   = ['a] Morbig.CSTVisitors.located_reduce2

let dummily_located value =
  { value ; position = Morbig.CSTHelpers.dummy_position }

let copy_location : 'a 'b. 'a located -> 'b -> 'b located =
  fun located value ->
  { value ; position = located.position }

let on_located f v =
  f v.value
