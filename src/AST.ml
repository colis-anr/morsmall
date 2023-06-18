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

(** A type alias for located pieces of AST. *)
(* NOTE: This type alias allows `ppx_import`-based modules to override the
   behaviour of the derivers when it comes to locations. *)
type 'a located = 'a Location.located

(** Names in Shell are just strings with a few additional
   conditions. *)

type name = string

(** The type {!word} is a description of words in Shell. {e See POSIX,
   2 Shell & Utilities, 2.3 Token Recognition} *)

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
  | WLiteral of string
  | WDoubleQuoted of word
  | WVariable of name * attribute
  | WSubshell of program
  | WGlobAll
  | WGlobAny
  | WBracketExpression (* FIXME *)

and word = word_component list
and word' = word located

(** For now, a {!pattern} is just a {!word}. *)

and pattern = word list
and pattern' = pattern located

(** An assignment is just a pair of a {!name} and a {!word}. *)

and assignment = name * word
and assignment' = assignment located

(** A file descriptor {!descr} is an integer. *)

and descr = int

(** The following description does contain all the semantic subtleties
   of POSIX Shell. Such a description can be found in the document
   {{:http://pubs.opengroup.org/onlinepubs/9699919799.2016edition/}IEEE
   Std 1003.1™-2008, 2016 Edition}. In the following, we will refer to
   it simple as POSIX.

   The type {!command} contains the definition of a Shell command in
   Morsmall. Compared to what can be found in the POSIX standard or in
   {{:https://github.com/colis-anr/morbig}Morbig}, this type is rather
   small. This is because a lot of syntactically distinct scripts that
   are semantically equivalent are identified in here. For instance,
   all the following scripts are equivalent and identified as such:
   {[
       if t1; then c1; elif t2; then c2; fi
       if { t1; }; then c1; elif { { t2; }; }; fi
       if t1; then c1; else if t2; then c2; fi; fi
   ]}

   {2 Simple Command}

   {e See POSIX, 2 Shell & Utilities, 2.9.1 Simple Command}

   {2 Lists and Pipelines}

   {e See POSIX, 2 Shell & Utilities, 2.9.2 Pipelines and 2.9.3 Lists}

   - {b Asynchronous Lists.} When encountering [Async c], the Shell
   shall execute [c] asynchronously in a subshell. This means that the
   shell shall not wait for the command to finish before executing the
   next command.

   - {b Sequential Lists.} {i A contrario}, the commands [c1] and [c2]
   in [Seq (c1, c2)] shall be executed sequentially.

   - {b AND Lists.} In [And (c1, c2)], [c1] shall be executed
   first. If its exit status is zero, [c2] shall be executed. The
   commands are expanded only if they are executed.

   - {b OR Lists.} In [Or (c1, c2)], [c1] shall be executed first. If
   its exit status is non-zero, [c2] shall be executed.

   - {b Pipeline.} In [Pipe (c1, c2)], the standard output of [c1]
   shall be connected to the standard input of [c2]. The standard
   input, standard output, or both of a command shall be considered to
   be assigned by the pipeline before any redirection specified by
   redirection operators that are part of the command.

   - {b Negation.} The command [Not c] has the same behaviour as [c],
   except for the exit status that shall be the logical NOT of the
   exit status of [c].

   {2 Compound Commands}

   {e See POSIX, 2 Shell & Utilities, 2.9.4 Compound Commands}

   - {b The Subshell Environment.} [Subshell c] shall execute [c] a
   subshell environment. Variable assignments and built-in commands
   that affect the environment shall not remain in effect after the
   list finishes.

   - {b The for Loop.} [For (x, l, c)] shall execute a sequence of
   commands [c] for each member in a list of items. It is to be noted
   that [l] is non-mandatory and is thus an option. Besides, there is
   an important semantic difference between [None] and [Some
   \[\]]. The former appears in a for loop where the list of words
   appear but is empty. In that case, the for loops through the empty
   list. The latter appears in a for loop where the list of words has
   been omitted. In that case, the for loops through the positional
   parameters.

   - {b The case Conditional Construct.} [Case (w, \[(\[p11;...\],c1);
   ...\])] shall execute the compound-list corresponding to the first
   one of several patterns that is matched by the string resulting
   from the expansion of the given word [w]. In order from the
   beginning to the end of the case statement, each pattern [p*] shall
   be subjected to expansion, and the result of these expansions shall
   be compared against the expansion of [w]. After the first match, no
   more patterns shall be expanded, and the corresponding [c*] shall
   be executed. The order of expansion and comparison of multiple
   patterns that label the same statement is unspecified.

   - {b The if Conditional Construct.} [If (c1, c2, c3)] shall execute
   [c1] and use its exit status to determine whether to execute [c2]
   or [c3]. In fact, [c3] is not mandatory and is thus an option.

   - {b The while Loop.} [While (c1, c2)] shall continuously execute
   [c2] as long as [c1] has a zero exit status.

   - {b The until Loop.} [Until (c1, c2)] shall continuously execute
   [c2] as long as [c1] has a non-zero exit status.

   {2 Function Definition Command}

   {e See POSIX, 2 Shell & Utilities, 2.9.5 Function Definition
   Command}

   A function is a user-defined name that is used as a simple command
   to call a compound command with new positional parameters. A
   function is defined with a {e function definition command},
   [Function (name, body)].

   This function definition command defines a function named [name:
   string] and with body [body: command]. The [body] shall be executed
   whenever [name] is specified as the name of a simple command.

   {2 Redirection}

   {e See POSIX, 2 Shell & Utilities, 2.7 Redirections}

 *)

(** {1 Type Definitions}

   The type [command] describes a command in the AST. All the command
   semantics are described at the top of this document. *)

and program = command' list

and command =
  (* Simple Commands *)
  | Simple of assignment' list * word' list

  (* Lists *)
  | Async of command'
  | Seq of command' * command'
  | And of command' * command'
  | Or of command' * command'

  (* Pipelines *)
  | Not of command'
  | Pipe of command' * command'

  (* Compound Command's *)
  | Subshell of command'
  | For of name * word' list option * command'
  | Case of word' * case_item' list
  | If of command' * command' * command' option
  | While of command' * command'
  | Until of command' * command'

  (* Function Definition Command' *)
  | Function of name * command'

  (* Redirection *)
  | Redirection of command' * descr * kind * word'
  | HereDocument of command' * descr * word'

and command' = command located

and case_item = pattern' * command' option

and case_item' = case_item located

and kind =
  | Output          (*  > *)
  | OutputDuplicate (* >& *)
  | OutputAppend    (* >> *)
  | OutputClobber   (* >| *)
  | Input           (*  < *)
  | InputDuplicate  (* <& *)
  | InputOutput     (* <> *)

(** {2 Smart Constructors} *)

(** {3 Attributes} *)

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

(** {3 Word Components} *)

(* FIXME: check with a regexp *)
let wTildePrefix str =
  WTildePrefix str

(* FIXME: check with a regexp *)
let wLiteral str =
  WLiteral str

let wVariable ?(attribute=noAttribute) name =
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

let simple ?(assignments=[]) words =
  if assignments = [] && words = [] then
    failwith "simple: assignments and words cannot both be empty";
  Simple (assignments, words)

let async c = c
let seq c1 c2 = Seq (c1, c2)
let and_ c1 c2 = And (c1, c2)
let or_ c1 c2 = Or (c1, c2)
let not_ c = Not c
let pipe c1 c2 = Pipe (c1, c2)
let subshell c = Subshell c
let for_ var ?words body = For (var, words, body)
let case word case_items = Case (word, case_items)
let if_ ?else_ ~then_ test = If (test, then_, else_)
let while_ test body = While (test, body)
let until test body = Until (test, body)
let function_ name body = Function (name, body)
let redirection command descr kind target = Redirection (command, descr, kind, target)

let hereDocument command descr content =
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
  HereDocument (command, descr, content)

(* let simple' ?(loc=Location.dummy) ?assignments words = *)
