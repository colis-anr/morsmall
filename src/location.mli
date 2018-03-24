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

type lexing_position = Libmorbig.CST.lexing_position =
  { pos_fname : string ;
    pos_lnum : int ;
    pos_bol : int ;
    pos_cnum : int }

type position = Libmorbig.CST.position =
  { start_p : lexing_position ;
    end_p : lexing_position }

type 'a located = 'a Libmorbig.CST.located =
  { value : 'a ;
    position : position }

val equal_located : ('a -> 'a -> bool) -> 'a located -> 'a located -> bool

val pp_located : (Format.formatter -> 'a -> unit)
                 -> Format.formatter -> 'a located -> unit

val map : ('a -> 'b) -> 'a located -> 'b located

val dummy_lexing_position : lexing_position
val dummy_position : position
val dummy_located : 'a -> 'a located
