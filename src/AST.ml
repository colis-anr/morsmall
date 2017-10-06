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

 and redirection_kind =
  | Output (* > *)
  | OutputDuplicate (* >& *)
  | OutputAppend (* >> *)
  | OutputClobber (* >| *)
  | Input (* < *)
  | InputDuplicate (* <& *)
  | InputOutput (* <> *)

 and command =
  | Async of command
  | Seq of command * command
  | And of command * command
  | Or of command * command
  | Not of command
  | Pipe of command * command
  | Subshell of command
  | If of command * command * command option
  | For of name * word list option * command
  | Case of word * (pattern * command option) list
  | While of command * command
  | Function of name * command
  | Assignment of assignment list
  | Simple of assignment list * word * word list
  | Redirection of command * int option * redirection_kind * word
  | HereDocument of command * int option * bool * word

[@@deriving
   visitors { variety = "iter";    polymorphic = true },
   visitors { variety = "map";     polymorphic = true },
   visitors { variety = "reduce";  polymorphic = true },
   visitors { variety = "iter2";   polymorphic = true },
   visitors { variety = "map2";    polymorphic = true },
   visitors { variety = "reduce2"; polymorphic = true }]
