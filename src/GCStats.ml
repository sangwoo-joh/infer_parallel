(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** Subset of {!Gc.Stat} that can be aggregated across multiple processes *)
type t =
  { minor_words: float
  ; promoted_words: float
  ; major_words: float
  ; minor_collections: int
  ; major_collections: int
  ; compactions: int
  ; top_heap_words: int }

type since =
  | ProgramStart  (** Get GC states from the beginning of the program *)
  | Previous of t
      (** Get GC states relative to another point in the time where GC states were obtained with
          [get ~since:ProgramStart]*)

let get ~since =
  let stats = Gc.stat () in
  match since with
  | ProgramStart ->
      { minor_words= stats.minor_words
      ; promoted_words= stats.promoted_words
      ; major_words= stats.major_words
      ; minor_collections= stats.minor_collections
      ; major_collections= stats.major_collections
      ; compactions= stats.compactions
      ; top_heap_words= stats.top_heap_words }
  | Previous prev ->
      { minor_words= stats.minor_words -. prev.minor_words
      ; promoted_words= stats.promoted_words -. prev.promoted_words
      ; major_words= stats.major_words -. prev.major_words
      ; minor_collections= stats.minor_collections - prev.minor_collections
      ; major_collections= stats.major_collections - prev.major_collections
      ; compactions= stats.compactions - prev.compactions
      ; top_heap_words=
          stats.top_heap_words
          (* [top_heap_words] is going to be inaccurate if it was reached in the previous time preiod *)
      }


let pp fmt
    ({ minor_words: float
     ; promoted_words: float
     ; major_words: float
     ; minor_collections: int
     ; major_collections: int
     ; compactions: int
     ; top_heap_words: int }[@warning "+9"]) =
  F.fprintf fmt
    "@[<v2>  minor_words: %g@;\
     promoted_words: %g@;\
     major_words: %g@;\
     minor_collections: %d@;\
     major_collections: %d@;\
     compactions: %d@;\
     top_heap_words: %d@;\
     @]"
    minor_words promoted_words major_words minor_collections major_collections compactions
    top_heap_words


let log ~name t = L.debug Quiet "@[GC stats for %s:@\n%a@]@." name pp t
