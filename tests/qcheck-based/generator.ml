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

open Morsmall
open AST
open QCheck2

module Gen = struct
  include Gen

  let map4 f g1 g2 g3 g4 =
    f <$> g1 <*> g2 <*> g3 <*> g4

  let sized (s : int) (gen_0 : 'a t) (gen_n : int -> 'a t) : 'a t =
    if s <= 0 then gen_0 else gen_n (s - 1)

  let rec reject ~(keep_if : 'a -> bool) (gen : 'a t) : 'a t =
    gen >>= fun x ->
    if keep_if x then
      pure x
    else
      reject ~keep_if gen
end

(* Infix synonyms for `map` and `ap`. *)

let (>|=) = Gen.(>|=)
let (<$>) = Gen.(<$>)
let (<*>) = Gen.(<*>)

let keywords = [ "for"; "in"; "do"; "done"; "if"; "then"; "else"; "fi"; "while";
                 "case"; "esac"; "elif"; "until" ]

let rec gen_name : name Gen.t =
  Gen.reject
    ~keep_if:(fun name -> not (List.mem name keywords))
    Gen.(string_size ~gen:(char_range 'a' 'z') (int_range 1 20))

and gen_attribute : attribute Gen.sized = fun s ->
  Gen.sized
    s
    (
      Gen.oneof [
        Gen.pure noAttribute ;
        Gen.pure parameterLength ;
      ]
    )
    (
      fun s ->
        Gen.oneof [
          Gen.map2 (fun also_for_null -> useDefaultValues ~also_for_null) Gen.bool (gen_word s) ;
          Gen.map2 (fun also_for_null -> assignDefaultValues ~also_for_null) Gen.bool (gen_word s) ;
          Gen.map2 (fun also_for_null -> indicateErrorifNullorUnset ~also_for_null) Gen.bool (gen_word s) ;
          Gen.map2 (fun also_for_null -> useAlternativeValue ~also_for_null) Gen.bool (gen_word s) ;
          removeSmallestSuffixPattern <$> gen_word s ;
          removeLargestSuffixPattern <$> gen_word s ;
          removeSmallestPrefixPattern <$> gen_word s ;
          removeLargestPrefixPattern <$> gen_word s ;
        ]
    )

and gen_word_component : word_component Gen.sized = fun s ->
  Gen.sized
    s
    (
      Gen.oneof [
        Gen.pure wGlobAll ;
        Gen.pure wGlobAny ;
        (* Gen.pure WBracketExpression ; *)
        wTildePrefix <$> gen_name ; (* FIXME: better than `gen_name` *)
        wLiteral <$> gen_name ; (* FIXME: better than `gen_name` *)
      ]
    )
    (
      fun s ->
        Gen.oneof [
          wDoubleQuoted <$> gen_word s ;
          Gen.map2 (fun attribute -> wVariable ~attribute) (gen_attribute s) gen_name ;
          wSubshell <$> gen_program s ;
        ]
    )

and gen_word : word Gen.sized = fun s ->
  Gen.sized
    s
    (gen_word_component 0 >|= fun word_component -> [word_component])
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_word_component s)
    )

and gen_pattern : pattern Gen.sized = fun s ->
  Gen.sized
    s
    (Gen.pure [])
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_word s)
    )

and gen_assignment : assignment Gen.sized = fun s ->
  Gen.sized
    s
    (gen_name >|= fun name -> (name, []))
    (
      fun s ->
        Gen.pair gen_name (gen_word s)
    )

and gen_descr : descr Gen.t =
  Gen.small_nat

and gen_program : program Gen.sized = fun s ->
  Gen.sized
    s
    (Gen.pure [])
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.small_list (gen_command' s)
    )

and gen_command : command Gen.sized = fun s ->
  Gen.sized
    s
    (gen_word' 0 >|= fun word -> simple [word])
    (
      fun s ->
        Gen.oneof [
          Gen.(
            (* NOTE: [Simple] constructor must not carry two empty lists, so we
               make sure to generate at most one empty list between the two. *)
            (0 -- 1) >>= fun d ->
            map2
              (fun assignments words -> simple ~assignments words)
              (list_size (   d  -- 10) (gen_assignment' s))
              (list_size ((1-d) -- 10) (gen_word' s))
          ) ;
          case <$> (gen_word' s) <*> (Gen.small_list (gen_case_item' s)) ;
          async <$> (gen_command' s) ;
          seq <$> (gen_command' s) <*> (gen_command' s) ;
          and_ <$> (gen_command' s) <*> (gen_command' s) ;
          or_ <$> (gen_command' s) <*> (gen_command' s) ;
          not_ <$> (gen_command' s) ;
          pipe <$> (gen_command' s) <*> (gen_command' s) ;
          subshell <$> (gen_command' s) ;
          Gen.map3 (fun name words command -> for_ name ?words command) gen_name (Gen.option (Gen.small_list (gen_word' s))) (gen_command' s) ;
          Gen.map3 (fun test then_ else_ -> if_ test ~then_ ?else_) (gen_command' s) (gen_command' s) (Gen.option (gen_command' s)) ;
          while_ <$> (gen_command' s) <*> (gen_command' s) ;
          until <$> (gen_command' s) <*> (gen_command' s) ;
          function_ <$> gen_name <*> (gen_command' s) ;
          redirection <$> (gen_command' s) <*> gen_descr <*> gen_kind <*> (gen_word' s) ;
          hereDocument <$> (gen_command' s) <*> gen_descr <*> (gen_word' s) ;
        ]
    )

and gen_case_item : case_item Gen.sized = fun s ->
  (* FIXME: improper use of size *)
  Gen.pair (gen_pattern' s) (Gen.option (gen_command' s))

and gen_kind : kind Gen.t =
  Gen.oneof [
    Gen.pure output ;
    Gen.pure outputDuplicate ;
    Gen.pure outputAppend ;
    Gen.pure outputClobber ;
    Gen.pure input ;
    Gen.pure inputDuplicate ;
    Gen.pure inputOutput ;
  ]

and gen_word' = fun s -> Location.locate <$> gen_word s
and gen_pattern' = fun s -> Location.locate <$> gen_pattern s
and gen_assignment' = fun s -> Location.locate <$> gen_assignment s
and gen_command' = fun s -> Location.locate <$> gen_command s
and gen_case_item' = fun s -> Location.locate <$> gen_case_item s
