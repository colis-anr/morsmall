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

let dummy_lexing_position = Libmorbig.CSTHelpers.dummy_lexing_position
let dummy_position = Libmorbig.CSTHelpers.dummy_position

let dummy_located value =
  { value ; position = dummy_position }

let equal_located eq_a v1 v2 =
  eq_a v1.value v2.value

let pp_located pp_a fmt loc =
  pp_a fmt loc.value

let map upd loc =
  { loc with value = upd loc.value }
