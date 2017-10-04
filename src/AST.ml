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
type name = string

type assignment = {
    name : name ;
    word : word
  }
                
type redirection = {
    channel : int option ;
    container : container
  }
                 
 and container =
   | Here of {
       trim : bool ;
       word : word
     }
   | File of {
       kind : file_kind ;
       word : word
     }

 (*FIXME: those are syntactic names. we want semantic ones.*)
 and file_kind = Less | LessAnd | Great | GreatAnd | DGreat | LessGreat | Clobber
           
          
type command =
  | Nop

  | Async of command

  | Seq of command * command

  | And of command * command
  | Or of command * command

  | Not of command

  | Pipe of command * command

  | Subshell of command

  | If of {
      test : command ;
      then_branch : command ;
      else_branch : command
    }

  | For of {
      name : name ;
      word_list : word list option ;
      body : command
    }

  | Case of {
      word : word ;
      cases : case list
    }

  | While of {
      test : command ;
      body : command
    }
 (* no until, it is replaced by while not *)

  | Function of {
      name : name ;
      body : command
    }

  | Simple of {
      assignments : assignment list ;
      words : word list ;
      redirections : redirection list
    }

  | Redirection of {
      command : command ;
      redirection : redirection
    }


                 
 and case = {
     pattern : word list ;
     body : command
   }
