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

type position = Location.MorbigLocation.position

module LAST = AST.LAST
(** Shell AST with locations. *)

module AST = AST.AST
(** Shell AST without locations. *)

(** {2 Parsers} *)

exception SyntaxError of position * string

val parse_file : string -> LAST.command list
(** Parses a whole Shell file into a list of {!LAST.command}. The list
   can be empty. Can raise {!SyntaxError}. *)

val strip_locations : LAST.command -> AST.command

(** {2 Printers} *)

val pp_print_safe : Format.formatter -> AST.command -> unit
(** Prints a Shell from its AST. *)

val pp_print_debug : Format.formatter -> AST.command -> unit
(** Prints a representation of the AST in OCaml-style. *)

(** {2 Interpreters} *)

module Env = Env
  
val interpret : Env.t -> AST.command -> Env.t
