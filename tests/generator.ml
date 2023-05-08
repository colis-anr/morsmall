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

open Morsmall
open AST
open QCheck2

let gen_map4 f g1 g2 g3 g4 =
  Gen.map (fun (x1, x2, x3, x4) -> f x1 x2 x3 x4) (Gen.quad g1 g2 g3 g4)

let gen_sized (s : int) (gen_0 : 'a Gen.t) (gen_n : int -> 'a Gen.t) : 'a Gen.t =
  if s <= 0 then gen_0 else gen_n (s - 1)

let rec gen_name : name Gen.t =
  Gen.(string_small_of (char_range 'a' 'z'))

and gen_attribute : attribute Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.oneof [
        Gen.pure NoAttribute ;
        Gen.pure ParameterLength ;
      ]
    )
    (
      fun s ->
        Gen.oneof [
          Gen.map2 (fun word bool -> UseDefaultValues (word, bool)) (gen_word s) Gen.bool ;
          Gen.map2 (fun word bool -> AssignDefaultValues (word, bool)) (gen_word s) Gen.bool ;
          Gen.map2 (fun word bool -> IndicateErrorifNullorUnset (word, bool)) (gen_word s) Gen.bool ;
          Gen.map2 (fun word bool -> UseAlternativeValue (word, bool)) (gen_word s) Gen.bool ;
          Gen.map (fun word -> RemoveSmallestSuffixPattern word) (gen_word s) ;
          Gen.map (fun word -> RemoveLargestSuffixPattern word) (gen_word s) ;
          Gen.map (fun word -> RemoveSmallestPrefixPattern word) (gen_word s) ;
          Gen.map (fun word -> RemoveLargestPrefixPattern word) (gen_word s) ;
        ]
    )

and gen_word_component : word_component Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.oneof [
        Gen.pure WGlobAll ;
        Gen.pure WGlobAny ;
        (* Gen.pure WBracketExpression ; *)
        Gen.map (fun string -> WTildePrefix string) gen_name ; (* FIXME: better than `gen_name` *)
        Gen.map (fun string -> WLiteral string) gen_name ; (* FIXME: better than `gen_name` *)
      ]
    )
    (
      fun s ->
        Gen.oneof [
          Gen.map (fun word -> WDoubleQuoted word) (gen_word s) ;
          Gen.map2 (fun name attribute -> WVariable (name, attribute)) gen_name (gen_attribute s) ;
          Gen.map (fun program -> WSubshell program) (gen_program s) ;
        ]
    )

and gen_word : word Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.pure []
    )
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_word_component s)
    )

and gen_pattern : pattern Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.pure []
    )
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_word s)
    )

and gen_assignment : assignment Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.map (fun name -> (name, [])) gen_name
    )
    (
      fun s ->
        Gen.pair gen_name (gen_word s)
    )

and gen_descr : descr Gen.t =
  Gen.small_nat

and gen_program : program Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.pure []
    )
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_command' s)
    )

and gen_command : command Gen.sized = fun s ->
  gen_sized
    s
    (
      Gen.map (fun word -> Simple ([], [word])) (gen_word' s)
    )
    (
      fun s ->
        Gen.oneof [
          Gen.map2 (fun assignments words -> Simple (assignments, words)) (Gen.small_list (gen_assignment' s)) (Gen.small_list (gen_word' s)) ;
          Gen.map2 (fun word case_items -> Case (word, case_items)) (gen_word' s) (Gen.small_list (gen_case_item' s)) ;
          Gen.map (fun command -> Async command) (gen_command' s) ;
          Gen.map2 (fun command1 command2 -> Seq (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map2 (fun command1 command2 -> And (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map2 (fun command1 command2 -> Or (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map (fun command -> Not command) (gen_command' s) ;
          Gen.map2 (fun command1 command2 -> Pipe (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map (fun command -> Subshell command) (gen_command' s) ;
          Gen.map3 (fun name words command -> For (name, words, command)) gen_name (Gen.option (Gen.small_list (gen_word' s))) (gen_command' s) ;
          Gen.map3 (fun command1 command2 command3 -> If (command1, command2, command3)) (gen_command' s) (gen_command' s) (Gen.option (gen_command' s)) ;
          Gen.map2 (fun command1 command2 -> While (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map2 (fun command1 command2 -> Until (command1, command2)) (gen_command' s) (gen_command' s) ;
          Gen.map2 (fun name command -> Function (name, command)) gen_name (gen_command' s) ;
          gen_map4 (fun command descr kind word -> Redirection (command, descr, kind, word)) (gen_command' s) gen_descr gen_kind (gen_word' s) ;
          Gen.map3 (fun command descr word -> HereDocument (command, descr, word)) (gen_command' s) gen_descr (gen_word' s) ;
        ]
    )

and gen_case_item : case_item Gen.sized = fun s ->
  (* FIXME: improper use of size *)
  Gen.pair (gen_pattern' s) (Gen.option (gen_command' s))

and gen_kind : kind Gen.t =
  Gen.oneof [
    Gen.pure Output ;
    Gen.pure OutputDuplicate ;
    Gen.pure OutputAppend ;
    Gen.pure OutputClobber ;
    Gen.pure Input ;
    Gen.pure InputDuplicate ;
    Gen.pure InputOutput ;
  ]

and gen_word' = fun s -> Gen.map Location.dummily_located (gen_word s)
and gen_pattern' = fun s -> Gen.map Location.dummily_located (gen_pattern s)
and gen_assignment' = fun s -> Gen.map Location.dummily_located (gen_assignment s)
and gen_command' = fun s -> Gen.map Location.dummily_located (gen_command s)
and gen_case_item' = fun s -> Gen.map Location.dummily_located (gen_case_item s)
