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

  (** Tries to map the exception-throwing function on the results of a generator
      a certain amount of times. If it keeps throwing exceptions, fall back on
      the other given generator. *)
  let rec map_retry ?(max_retries=5) ~(fallback : 'b t) (f : 'a -> 'b) (gen : 'a t) : 'b t =
    if max_retries < 0 then
      fallback
    else
      gen >>= fun x ->
      try pure (f x) with _ -> map_retry ~max_retries:(max_retries - 1) ~fallback f gen

  let map2_retry ?max_retries ~fallback f g1 g2 =
    map_retry ?max_retries ~fallback (fun (x1, x2) -> f x1 x2) (pair g1 g2)

  let map3_retry ?max_retries ~fallback f g1 g2 g3 =
    map_retry ?max_retries ~fallback (fun (x1, x2, x3) -> f x1 x2 x3) (triple g1 g2 g3)

  let very_small_nat = 0 -- 10
  let very_small_list gen = list_size very_small_nat gen

  let singleton g = g >|= fun x -> [x]
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
          Gen.map_retry wDoubleQuoted (gen_word s)
            ~fallback:(wDoubleQuoted <$> (Gen.singleton (wLiteral <$> gen_name))) ;
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
        Gen.very_small_list (gen_word_component s)
    )

and gen_pattern : pattern Gen.sized = fun s ->
  Gen.sized
    s
    (Gen.pure [])
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.very_small_list (gen_word s)
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
  Gen.(0 -- 9)

and gen_program : program Gen.sized = fun s ->
  Gen.sized
    s
    (Gen.pure [])
    (
      fun s ->
        (* FIXME: improper use of size *)
        Gen.very_small_list (gen_command' s)
    )

and gen_command : command Gen.sized = fun s ->
  Gen.sized
    s
    (gen_word' 0 >|= fun word -> simple [word])
    (
      fun s ->
        Gen.oneof [
          Gen.map2_retry (fun assignments -> simple ~assignments) (Gen.very_small_list (gen_assignment' s)) (Gen.very_small_list (gen_word' s))
            ~fallback:(simple ~assignments:[] <$> (Gen.singleton (gen_word' s))) ;
          case <$> gen_word' s <*> Gen.very_small_list (gen_case_item' s) ;
          async <$> gen_command' s ;
          seq <$> gen_command' s <*> gen_command' s ;
          and_ <$> gen_command' s <*> gen_command' s ;
          or_ <$> gen_command' s <*> gen_command' s ;
          not_ <$> gen_command' s ;
          pipe <$> gen_command' s <*> gen_command' s ;
          subshell <$> gen_command' s ;
          Gen.map3 (fun name words command -> for_ name ?words command) gen_name (Gen.option (Gen.very_small_list (gen_word' s))) (gen_command' s) ;
          Gen.map3 (fun test then_ else_ -> if_ test ~then_ ?else_) (gen_command' s) (gen_command' s) (Gen.option (gen_command' s)) ;
          while_ <$> gen_command' s <*> gen_command' s ;
          until <$> gen_command' s <*> gen_command' s ;
          function_ <$> gen_name <*> gen_command' s ;
          (fun around -> redirection ~around) <$> gen_command' s <*> gen_descr <*> gen_kind <*> gen_word' s ;
          Gen.map3_retry (fun around -> hereDocument ~around) (gen_command' s) gen_descr (gen_word' s)
            ~fallback:((fun around -> hereDocument ~around) <$> gen_command' s <*> gen_descr <*> Gen.pure (Location.locate [])) ;
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
