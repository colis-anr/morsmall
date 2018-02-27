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

let fpf = Format.fprintf
open AST

let pp_word ppf = fpf ppf "%s"

let rec pp_words ppf = function
  | [] -> ()
  | [word] ->
     pp_word ppf word
  | word :: words ->
     fpf ppf "%a %a"
       pp_word word
       pp_words words

let rec pp_pattern ppf = function
  | [] -> ()
  | [word] ->
     pp_word ppf word
  | word :: pattern ->
     fpf ppf "%a|%a"
       pp_word word
       pp_pattern pattern

let pp_name ppf = fpf ppf "%s"

let pp_assignment ppf (name, word) =
  fpf ppf "%a=%a"
    pp_name name
    pp_word word

let rec pp_assignments ppf = function
  | [] -> ()
  | [assignment] ->
     pp_assignment ppf assignment
  | assignment :: assignments ->
     fpf ppf "%a %a"
       pp_assignment assignment
       pp_assignments assignments

let pp_redirection_kind ppf k =
  fpf ppf "%s"
    (match k with
     | Input -> "<" | InputDuplicate -> "<&"
     | Output -> ">" | OutputDuplicate -> ">&" | OutputAppend -> ">>"
     | InputOutput -> "<>" | OutputClobber -> ">|")

let rec pp_command ppf command =
  fpf ppf "{ ";
  (
    match command with

    | Async command ->
       pp_command ppf command

    | Seq (command1, command2) ->
       fpf ppf "%a;%a"
         pp_command command1
         pp_command command2

    | And (command1, command2) ->
       fpf ppf "%a&&%a"
         pp_command command1
         pp_command command2

    | Or (command1, command2) ->
       fpf ppf "%a||%a"
         pp_command command1
         pp_command command2

    | Not command ->
       fpf ppf "! %a"
         pp_command command

    | Pipe (command1, command2) ->
       fpf ppf "%a|%a"
         pp_command command1
         pp_command command2

    | Subshell command ->
       fpf ppf "(%a)"
         pp_command command

    | If (test, then_branch, None) ->
       fpf ppf "if %a;then %a;fi"
         pp_command test
         pp_command then_branch
    | If (test, then_branch, Some else_branch) ->
       fpf ppf "if %a;then %a;else %a;fi"
         pp_command test
         pp_command then_branch
         pp_command else_branch

    | For (name, None, body) ->
       fpf ppf "for %a;do %a;done"
         pp_name name
         pp_command body
    | For (name, Some words, body) ->
       fpf ppf "for %a in %a;do %a;done"
         pp_name name
         pp_words words
         pp_command body

    | Case (word, items) ->
       fpf ppf "case %a in" pp_word word;
       List.iter
         (function
          | (pattern, None) -> fpf ppf " %a) ;;" pp_pattern pattern
          | (pattern, Some body) -> fpf ppf " %a) %a;;" pp_pattern pattern pp_command body)
         items;
       fpf ppf "esac"

    | While (test, body) ->
       fpf ppf "while %a;do %a;done"
         pp_command test
         pp_command body

    | Until (test, body) ->
       fpf ppf "until %a;do %a;done"
         pp_command test
         pp_command body
      
    | Function (name, body) ->
       fpf ppf "%a()%a"
         pp_name name
         pp_command body

    | Simple ([], []) ->
       failwith "SafePrinter.pp_command: ill-formed command: Simple([], [])"
    | Simple ([], words) ->
       fpf ppf "%a" pp_words words
    | Simple (assignments, words) ->
       fpf ppf "%a %a"
         pp_assignments assignments
         pp_words words

    | Redirection (command, channel, kind, word) ->
       fpf ppf "%a%s%a%a" (*WARNING: no space required only because we print a '}' at the end of each command*)
         pp_command command
         (match channel with None -> "" | Some channel -> string_of_int channel)
         pp_redirection_kind kind
         pp_word word

    | HereDocument (command, channel, trim, content) ->
       let eof = "EOF" in (*FIXME*)
       fpf ppf "%a%s%s%s\n%a\n%s\n"
         pp_command command
         (match channel with None -> "" | Some channel -> string_of_int channel)
         (if trim then "<<-" else "<<")
         eof
         pp_word content
         eof
  );
  fpf ppf "%s}" (match command with Async _ -> "&" | HereDocument _ -> "" | _ -> ";")
