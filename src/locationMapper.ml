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

module LAST = AST.LAST
module AST = AST.AST

let optmap f = function
  | None -> None
  | Some x -> Some (f x)

let strip_location f (l : 'a LAST.located) =
  f l.Location.MorbigLocation.value

let rec name n = n

and character_range cl = cl

and word_component = function
  | LAST.Literal s -> AST.Literal s
  | LAST.DoubleQuoted w -> AST.DoubleQuoted (word w)
  | LAST.Variable v -> AST.Variable v
  | LAST.Subshell cl -> AST.Subshell (List.map command cl)
  | LAST.Name n -> AST.Name (name n)
  | LAST.Assignment a -> AST.Assignment (assignment a)
  | LAST.GlobAll -> AST.GlobAll
  | LAST.GlobAny -> AST.GlobAny
  | LAST.GlobRange cr -> AST.GlobRange (character_range cr)

and word w = List.map word_component w

and word' w' = strip_location word w'

and pattern p = List.map word p

and pattern' p' = strip_location pattern p'

and assignment a =
  AST.{ variable = name a.LAST.variable ;
        word = word a.LAST.word }

and assignment' a' = strip_location assignment a'

and descr i = i

and command = function
  | LAST.Simple (al, wl) ->
     AST.Simple (List.map assignment' al, List.map word' wl)

  | LAST.Async c -> command c
  | LAST.Seq (c1, c2) -> AST.Seq (command' c1, command' c2)
  | LAST.And (c1, c2) -> AST.And (command' c1, command' c2)
  | LAST.Or (c1, c2) -> AST.Or (command' c1, command' c2)

  | LAST.Not c -> AST.Not (command' c)
  | LAST.Pipe (c1, c2) -> AST.Pipe (command' c1, command' c2)

  | LAST.Subshell c -> AST.Subshell (command' c)

  | LAST.For (n, None, c) ->
     AST.For (name n, None, command' c)
  | LAST.For (n, Some wl, c) ->
     AST.For (name n, Some (List.map word wl), command' c)

  | LAST.Case (w, cil) ->
     AST.Case (word w, List.map case_item' cil)
  | LAST.If (c1, c2, None) ->
     AST.If (command' c1, command' c2, None)
  | LAST.If (c1, c2, Some c3) ->
     AST.If (command' c1, command' c2, Some (command' c3))
  | LAST.While (c1, c2) ->
     AST.While (command' c1, command' c2)
  | LAST.Until (c1, c2) ->
     AST.Until (command' c1, command' c2)

  | LAST.Function (n, c) ->
     AST.Function (name n, command' c)

  | LAST.Redirection (c, d, k, w) ->
     AST.Redirection (command' c, descr d, kind k, word w)
  | LAST.HereDocument (c, d, w) ->
     AST.HereDocument (command' c, descr d, word' w)

and command' c' = strip_location command c'

and case_item ci =
  AST.{ pattern = pattern' ci.LAST.pattern ;
        body = optmap command' ci.LAST.body }

and case_item' ci' = strip_location case_item ci'

and kind = function
  | LAST.Output -> AST.Output
  | LAST.OutputDuplicate -> AST.OutputDuplicate
  | LAST.OutputAppend -> AST.OutputAppend
  | LAST.OutputClobber -> AST.OutputClobber
  | LAST.Input -> AST.Input
  | LAST.InputDuplicate -> AST.InputDuplicate
  | LAST.InputOutput -> AST.InputOutput
