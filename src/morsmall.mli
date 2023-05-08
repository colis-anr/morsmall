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
(** The AST of Shell scripts, as provided by Morsmall. This is a stripped down
    version of the CST provided by Morbig. *)

type t = AST.program
(** A type alias for a Shell program. *)

(** {2 Parsers and Converters} *)

exception SyntaxError of Location.lexing_position

val from_CST : Morbig.CST.program -> t

val parse_file : string -> t
(** Parses a whole Shell file into an {!AST.program}. Can raise
    {!SyntaxError}. *)

(** {2 Equalities} *)

val equal_program : t -> t -> bool
(** Check that two programs are equal. This takes into account the locations of
    the various elements of the AST. *)

val equal_program_noloc : t -> t -> bool
(** Check that two programs are equal, ignoring the locations of the various
    elements of the AST. *)

(** {2 Printers} *)

val pp_print_safe : Format.formatter -> t -> unit
(** Prints a Shell from its AST. *)

val pp_print_json : Format.formatter -> t -> unit
(** Prints a representation of the AST in JSON. *)

val pp_print_json_noloc : Format.formatter -> t -> unit
(** Prints a representation of the AST in JSON, ignoring the locations of the
    various elements of the AST. *)

val pp_print_debug : Format.formatter -> t -> unit
(** Prints a representation of the AST in OCaml-style. *)

module Printer : sig
  module Safe = SafePrinter
  module Json = JsonPrinter
  module Debug = DebugPrinter
end

(** {2 Other Modules} *)

module Location = Location
module CST_to_AST = CST_to_AST
module Utilities = Morsmall_utilities
module Visitors = Visitors

module Equality : sig
  module Located = LocatedEquality
end
