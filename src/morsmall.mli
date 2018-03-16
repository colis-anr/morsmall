
module AST = AST

module Converter = Converter

val cst_to_ast : Libmorbig.CST.complete_command -> AST.command option
(** Converts a CST from Morbig into an AST *)

val pp_print_safe : Format.formatter -> AST.command -> unit
(** Prints a Shell from its AST. *)

val pp_print_debug : Format.formatter -> AST.command -> unit
(** Prints a representation of the AST in OCaml-style. *)
