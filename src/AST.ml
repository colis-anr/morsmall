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

(** This module contains an AST for POSIX Shell. *)

type 'a located =
  { value : 'a ;
    pos_start : Lexing.position ;
    pos_end : Lexing.position }

(** The type {!word} is a (for now quite concrete, but soon abstract)
   description of words in Shell. {e See POSIX, 2 Shell & Utilities,
   2.3 Token Recognition} *)

type word = string
type word' = word located
(* type word_list' = word list located *)

(** Names in Shell are just strings with a few additional
   conditions. *)

type name = string
(* type name' = name located *)

(** For now, a {!pattern} is just a {!word}. *)

type pattern = word list
 type pattern' = pattern located

(** An assignment is just a pair of a {!name} and a {!word}. *)

type assignment =
  { name : name ;
    word : word }
type assignment' = assignment located

(** A file descriptor {!descr} is an integer. *)

type descr = int
(* type descr' = descr located *)

(** The different kinds of redirection. *)

type redirection_kind =
  | Output          (*  > *)
  | OutputDuplicate (* >& *)
  | OutputAppend    (* >> *)
  | OutputClobber   (* >| *)
  | Input           (*  < *)
  | InputDuplicate  (* <& *)
  | InputOutput     (* <> *)
(* type redirection_kind' = redirection_kind located *)

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

type command =
  (* Simple Commands *)
  | Simple of simple_command

  (* Lists *)
  | Async of command
  | Seq of command' * command'
  | And of command' * command'
  | Or of command' * command'

  (* Pipelines *)
  | Not of command'
  | Pipe of command' * command'

  (* Compound Command's *)
  | Subshell of command'
  | For of for_clause
  | Case of case_clause
  | If of if_clause
  | While of while_clause
  | Until of until_clause

  (* Function Definition Command' *)
  | Function of function_definition

  (* Redirection *)
  | Redirection of command' * redirection
  | HereDocument of command' * here_document

and command' = command located

and simple_command =
  { assignments : assignment' list ;
    words : word' list }

and for_clause =
  { variable : name ;
    words : word list option ;
    body : command' }

and case_clause =
  { word : word' ;
    items : case_item list }

and case_item =
  { patterns : pattern' ;
    body : command' option }

and if_clause =
  { test : command' ;
    body : command' ;
    other : command' option }

and while_clause =
  { test : command' ;
    body : command' }

and until_clause = while_clause

and function_definition =
  { name : name ;
    body : command' }

and redirection =
  { descr : descr option ;
    kind : redirection_kind ;
    file : word }

and here_document =
  { descr : descr option ;
    globber : bool ;
    content : word' }
