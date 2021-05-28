(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t

type since =
  | ProgramStart  (** Get GC states from the beginning of the program *)
  | Previous of t
      (** Get GC states relative to another point in the time where GC states were obtained with
          [get ~since:ProgramStart]*)

val get : since:since -> t

val log : name:string -> t -> unit
