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

open AST

let dummy_position =
  Lexing.{
      pos_fname = "dummy" ;
      pos_lnum = 0 ;
      pos_bol = 0 ;
      pos_cnum = 0 }

let dummy_locate f x =
  { value = f x ;
    pos_start = dummy_position ;
    pos_end = dummy_position }

type 'a p_array = (int * 'a) array

let choose (a : 'a p_array) : 'a =
  let p_max = Array.fold_left (fun p_tot (p,_) -> p_tot+p) 0 a in
  let n = Random.int p_max in
  let p_tot = ref 0 in
  let v_found = ref (snd (a.(0))) in
  for i = 0 to Array.length a - 1 do
    let (p, v) = a.(i) in
    if !p_tot <= n && n < !p_tot + p then
      v_found := v;
    p_tot := !p_tot + p
  done;
  !v_found

(* Parameters *)

type parameters =
  { depth : int }

let d p = { p with depth = p.depth - 1 }

(* Generator helper functions *)

let g_bool ~prob =
  if Random.float 1. < prob then
    true
  else
    false

let g_option ~prob inhabitant =
  if Random.float 1. < prob then
    Some (inhabitant ())
  else
    None

let rec g_list ~prob ~limit inhabitant =
  if limit > 0 && Random.float 1. < prob then
    (inhabitant ()) :: g_list ~prob ~limit:(limit - 1) inhabitant
  else
    []

(* Our generators *)

let g_word _p =
  "echo" (*FIXME*)

let g_word' p =
  dummy_locate g_word p

let g_name _p =
  "blah" (*FIXME*)

let g_pattern p =
  g_list ~prob:0.8 ~limit:4 (fun () -> g_word (d p))

let g_pattern' p =
  dummy_locate g_pattern p

let g_assignment p =
  { variable = choose [|1,"x";2,"y";3,"z";4,"choucroute"|] ;
    word = g_word (d p) }

let g_assignment' p =
  dummy_locate g_assignment p

let g_descr _p =
  g_option
    ~prob:0.5
    (fun () -> Random.int 10)

let g_redirection_kind _p =
  choose
    [| 1, Output ;
       1, OutputDuplicate ;
       1, OutputAppend ;
       1, OutputClobber ;
       1, Input ;
       1, InputDuplicate ;
       1, InputOutput |]

let g_redirection p =
  { descr = g_descr (d p) ;
    kind = g_redirection_kind (d p) ;
    file = g_word (d p) }

let g_here_document p =
  { descr = g_descr (d p) ;
    strip = g_bool ~prob:0.5 ;
    content = g_word' (d p) }

let rec g_command p =
  if p.depth <= 0 then
    g_simple_command (d p)
  else
    choose
      [| 1, g_simple_command ;
         1, (fun p -> Async (g_command (d p))) ;
         1, (fun p -> Seq (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> And (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Or (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Not (g_command' (d p))) ;
         1, (fun p -> Pipe (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Subshell (g_command' (d p))) ;
         1, g_for_clause ;
         1, g_case_clause ;
         1, g_if_clause ;
         1, g_while_clause ;
         1, g_until_clause ;
         1, g_function_definition ;
         1, (fun p -> Redirection (g_command' (d p), g_redirection (d p))) ;
         1, (fun p -> HereDocument (g_command' (d p), g_here_document (d p))) |]
      (d p)

and g_command' p =
  dummy_locate g_command p

and g_simple_command p =
  let assignments =
    g_list ~prob:0.5 ~limit:5
      (fun () -> g_assignment' (d p))
  in
  let words =
    g_list ~prob:0.7 ~limit:10
      (fun () -> g_word' (d p))
  in
  if assignments = [] && words = [] then
    g_simple_command p
  else
    Simple { assignments ; words }

and g_for_clause p =
  For {
      variable = "x" ; (*FIXME*)
      words =
        g_option ~prob:0.8
          (fun () -> g_list ~prob:0.8 ~limit:10
                       (fun () -> g_word (d p))) ;
      body = g_command' (d p)
    }

and g_case_clause p =
  Case {
      word = "x" ;
      items =
        g_list ~prob:0.7 ~limit:5
          (fun () -> g_case_item' (d p) )
    }

and g_case_item p =
  { pattern = g_pattern' (d p) ;
    body = g_option ~prob:0.9 (fun () -> g_command' (d p)) }

and g_case_item' p =
  dummy_locate g_case_item p

and g_if_clause p =
  If { test = g_command' (d p) ;
       body = g_command' (d p) ;
       rest = g_option ~prob:0.6 (fun () -> g_command' (d p)) }

and g_while_clause p =
  While { test = g_command' (d p) ;
          body = g_command' (d p) }

and g_until_clause p =
  Until { test = g_command' (d p) ;
          body = g_command' (d p) }

and g_function_definition p =
  Function { name = g_name (d p) ;
             body = g_command' (d p) }
