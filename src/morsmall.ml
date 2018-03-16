
module AST = AST

module Converter = Converter

let cst_to_ast =
  Converter.complete_command__to__command_option

let pp_print_safe = SafePrinter.pp_command
let pp_print_debug fmt command =
  Format.pp_print_string fmt (AST.show_command command)
