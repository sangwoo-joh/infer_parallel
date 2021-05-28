(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception
  (* WARNING: ONLY USE IF Logging IS NOT AVAILABLE TO YOU FOR SOME REASON (e.g., inside Config) *)
    ExternalError of
    string

exception InternalError of string

exception UserError of string

type error = ExternalError | InternalError | UserError

val raise_error : ?backtrace:Caml.Printexc.raw_backtrace -> error -> msg:string -> 'a

val die : error -> ('a, Format.formatter, unit, _) format4 -> 'a
(** Raise the corresponding exception *)
