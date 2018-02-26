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

type word = string
and name = string
and pattern = word list
and assignment = name * word
and file_descr = int option

and redirection_kind =
  | Output          (*  > *)
  | OutputDuplicate (* >& *)
  | OutputAppend    (* >> *)
  | OutputClobber   (* >| *)
  | Input           (*  < *)
  | InputDuplicate  (* <& *)
  | InputOutput     (* <> *)

and command =
  | Empty

  | Async of command
  (*  Async c          ~=  c &                          *)
  | Seq of command * command
  (*  Seq (c1, c2)     ~=  c1 ;  c2                     *)
  | And of command * command
  (*  And (c1, c2)     ~=  c1 && c2                     *)
  | Or of command * command
  (*  Or  (c1, c2)     ~=  c1 || c2                     *)
  | Not of command
  (*  Not c            ~=  ! c                          *)
  | Pipe of command * command
  (*  Pipe (c1, c2)    ~=  c1  | c2                     *)
  | Subshell of command
  (*  Subshell c       ~=  ( c )                        *)
  | If of command * command * command
  (*  If (c1, c2, c3)  ~=  if c1; then c2; else c3; fi  *)

  | For of name * word list option * command
  (*  For (x, l, c)    ~=  for x in l; do c; done       *)

  | Case of word * (pattern * command) list
  (*
      Case (x, [ (p1, c1); (p2, c2); ... ])
      ~=  case x in
              p1) c1;;
              p2) c2;;
              ...
          esac
   *)

  | While of command * command
  (*  While (c1, c2)  ~=  while c1; do c2; done  *)
  | Until of command * command
  (*  Until (c1, c2)  ~=  until c1; do c2; done  *)

  | Function of name * command
  (*  Function (n, c)  ~=  n () c  *)

  | Simple of assignment list * word list
  (*
      Simple ([ (n1, w1); (n2, w2); ... ], [w1'; w2'; ...])
      ~= n1=w1 n2=w2 ... w1' w2' ...
   *)

  | Redirection of command * file_descr * redirection_kind * word
  (*  Redirection (c, n, k, w)  ~=  c n k w  *)

  | HereDocument of command * file_descr * bool * word
  (*  HereDocument (c, n, b, w)  ~=  c n <<b w  *)
