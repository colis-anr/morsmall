(******************************************************************************)
(*                                  Morsmall                                  *)
(*                       A concise AST for POSIX shell                        *)
(*                                                                            *)
(*   Copyright (C) 2017-2023 Yann Régis-Gianas, Ralf Treinen,                 *)
(*   Nicolas Jeannerod                                                        *)
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
(******************************************************************************)

(* Types *)

(* NOTE: This type alias allows `ppx_import`-based modules to override the
   behaviour of the derivers when it comes to locations. *)
type 'a located = 'a Location.located

(* NOTE: We copy the whole AST here. Ideally we could just use `ppx_import`, but
   this would also import the `private` flag, which we do not want. cf:
   https://github.com/ocaml-ppx/ppx_import/issues/13 . *)

type name = string

and attribute =
  | NoAttribute
  | ParameterLength
  | UseDefaultValues of word * bool
  | AssignDefaultValues of word * bool
  | IndicateErrorifNullorUnset of word * bool
  | UseAlternativeValue of word * bool
  | RemoveSmallestSuffixPattern of word
  | RemoveLargestSuffixPattern of word
  | RemoveSmallestPrefixPattern of word
  | RemoveLargestPrefixPattern of word

and word_component =
  | WTildePrefix of string
  | WUnquoted of string
  | WSingleQuoted of string
  | WDoubleQuoted of word
  | WVariable of name * attribute
  | WSubshell of program
  | WGlobAll
  | WGlobAny
  | WBracketExpression (* FIXME *)

and word = word_component list
and word' = word located

and pattern = word list
and pattern' = pattern located

and assignment = name * word
and assignment' = assignment located

and descr = int

and program = command' list

and command =
  | Simple of assignment' list * word' list
  | Async of command'
  | Seq of command' * command'
  | And of command' * command'
  | Or of command' * command'
  | Not of command'
  | Pipe of command' * command'
  | Subshell of command'
  | For of name * word' list option * command'
  | Case of word' * case_item' list
  | If of command' * command' * command' option
  | While of command' * command'
  | Until of command' * command'
  | Function of name * command'
  | Redirection of command' option * descr * kind * word'
  | HereDocument of command' option * descr * word * word'

and command' = command located

and case_item = pattern' * command' option

and case_item' = case_item located

and kind =
  | Output (* > *)
  | OutputDuplicate (* >& *)
  | OutputAppend (* >> *)
  | OutputClobber (* >| *)
  | Input (* < *)
  | InputDuplicate (* <& *)
  | InputOutput (* <> *)

(* Smart constructors *)

let noAttribute = NoAttribute
let parameterLength = ParameterLength
let useDefaultValues ~also_for_null word = UseDefaultValues (word, also_for_null)
let assignDefaultValues ~also_for_null word = AssignDefaultValues (word, also_for_null)
let indicateErrorifNullorUnset ~also_for_null word = IndicateErrorifNullorUnset (word, also_for_null)
let useAlternativeValue ~also_for_null word = UseAlternativeValue (word, also_for_null)
let removeSmallestSuffixPattern word = RemoveSmallestSuffixPattern word
let removeLargestSuffixPattern word = RemoveLargestSuffixPattern word
let removeSmallestPrefixPattern word = RemoveSmallestPrefixPattern word
let removeLargestPrefixPattern word = RemoveLargestPrefixPattern word

(* FIXME: check with a regexp *)
let wTildePrefix str =
  WTildePrefix str

(* FIXME: check with a regexp *)
let wUnquoted str =
  WUnquoted str

let wSingleQuoted str =
  WSingleQuoted str

let wVariable ?(attribute = noAttribute) name =
  WVariable (name, attribute)

let wSubshell program =
  WSubshell program

let wGlobAll = WGlobAll

let wGlobAny = WGlobAny

let wBracketExpression = WBracketExpression

let wDoubleQuoted word =
  List.iter
    (
      function
      | WTildePrefix _ -> failwith "wDoubleQuoted: cannot contain a tilde prefix"
      | WDoubleQuoted _ -> failwith "wDoubleQuoted: cannot contain a double-quoted word"
      | WGlobAll -> failwith "wDoubleQuoted: cannot contain a glob"
      | WGlobAny -> failwith "wDoubleQuoted: cannot contain a glob"
      | WBracketExpression -> failwith "wDoubleQuoted: cannot contain a bracket expression"
      | _ -> ()
    )
    word;
  WDoubleQuoted word

(** {3 Others} *)

let word components = components

(* FIXME: Probably can't be an empty list? And probably cannot contain empty
   words? *)
let pattern words = words

(* FIXME: Check the name. *)
let assignment name word = (name, word)

let descr n = n

let program commands = commands

(** {3 Commands} *)

let simple ?(assignments = []) words =
  if assignments = [] && words = [] then
    failwith "simple: assignments and words cannot both be empty";
  Simple (assignments, words)

let async c = Async c
let seq c1 c2 = Seq (c1, c2)
let and_ c1 c2 = And (c1, c2)
let or_ c1 c2 = Or (c1, c2)
let not_ c = Not c
let pipe c1 c2 = Pipe (c1, c2)
let subshell c = Subshell c
let for_ var ?words body = For (var, words, body)
let case word case_items = Case (word, case_items)
let if_ ~then_ ?else_ test = If (test, then_, else_)
let while_ test body = While (test, body)
let until test body = Until (test, body)
let function_ name body = Function (name, body)

let redirection ?around descr kind target =
  Redirection (around, descr, kind, target)

let hereDocument ?around ?(delimiter = [wUnquoted "EOF"]) descr content =
  List.iter
    (
      function
      | WTildePrefix _ -> failwith "hereDocument: cannot contain a tilde prefix"
      | WDoubleQuoted _ -> failwith "hereDocument: cannot contain a double-quoted word"
      | WGlobAll -> failwith "hereDocument: cannot contain a glob"
      | WGlobAny -> failwith "hereDocument: cannot contain a glob"
      | WBracketExpression -> failwith "hereDocument: cannot contain a bracket expression"
      | _ -> ()
    )
    content.Location.value;
  HereDocument (around, descr, delimiter, content)

(* let simple' ?(loc=Location.dummy) ?assignments words = *)

let output = Output
let outputDuplicate = OutputDuplicate
let outputAppend = OutputAppend
let outputClobber = OutputClobber
let input = Input
let inputDuplicate = InputDuplicate
let inputOutput = InputOutput
