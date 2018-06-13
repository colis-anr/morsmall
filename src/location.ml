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

module type Location =
  sig
    type 'a located

    val equal_located : ('a -> 'a -> bool) -> 'a located -> 'a located -> bool
    val pp_located : (Format.formatter -> 'a -> unit)
                     -> Format.formatter -> 'a located -> unit

    val dummily_located : 'a -> 'a located
  end

module NoLocation =
  struct
    type 'a located = 'a

    let equal_located equal_a a a' =
      equal_a a a'

    let pp_located pp_a fmt a =
      pp_a fmt a

    let dummily_located a = a
  end

module MorbigLocation =
  struct
    type lexing_position = Libmorbig.CST.lexing_position =
      { pos_fname : string ;
        pos_lnum : int ;
        pos_bol : int ;
        pos_cnum : int }                  [@@deriving eq, show{with_path=false}]

    type position = Libmorbig.CST.position =
      { start_p : lexing_position ;
        end_p : lexing_position }         [@@deriving eq, show{with_path=false}]

    type 'a located = 'a Libmorbig.CST.located =
      { value : 'a ;
        position : position }             [@@deriving eq, show{with_path=false}]

    let dummily_located value =
      { value ; position = Libmorbig.CSTHelpers.dummy_position }
  end
