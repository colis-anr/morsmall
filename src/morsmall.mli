
module AST = AST

module Converter = Converter

module Printer : sig
  module Safe = SafePrinter
end

val cst_to_ast : Libmorbig.CST.complete_command -> AST.command option
(** Converts a CST from Morbig into an AST *)

val pp_print_safe : Format.formatter -> AST.command -> unit
(** Prints a Shell from its AST *)
