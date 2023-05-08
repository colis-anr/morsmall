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

class virtual ['a] located_iter      = ['a] Morbig.CSTVisitors.located_iter
class virtual ['a] located_map       = ['a] Morbig.CSTVisitors.located_map
class virtual ['a] located_reduce    = ['a] Morbig.CSTVisitors.located_reduce
class virtual ['a] located_mapreduce = ['a] Morbig.CSTVisitors.located_mapreduce
class virtual ['a] located_iter2     = ['a] Morbig.CSTVisitors.located_iter2
class virtual ['a] located_map2      = ['a] Morbig.CSTVisitors.located_map2
class virtual ['a] located_reduce2   = ['a] Morbig.CSTVisitors.located_reduce2

type name = [%import: AST.name]
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

[@@deriving
  visitors {variety = "iter";       ancestors=["located_iter"];      nude=true},
  visitors {variety = "map";        ancestors=["located_map"];       nude=true},
  visitors {variety = "reduce";     ancestors=["located_reduce"];    nude=true},
  visitors {variety = "mapreduce";  ancestors=["located_mapreduce"]; nude=true},
  visitors {variety = "iter2";      ancestors=["located_iter2"];     nude=true},
  visitors {variety = "map2";       ancestors=["located_map2"];      nude=true},
  visitors {variety = "reduce2";    ancestors=["located_reduce2"];   nude=true}
]
