(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val oom_threshold : int option ref
(** Programmable threshold for out-of-memory. *)

val progress_bar : [`Multiline | `Plain | `Quiet] ref
(** The kind of progress bar. It is ok not to have multiline progress bar. *)

val jobs : int ref
(** The number of parallel workers. *)

val keep_going : bool ref
(** Whether or not to continue the full tasks even if some of the children had died. *)

type verbose_level = Quiet | Moderate | Verbose

val verbose_level : verbose_level ref

val verbose : unit -> bool

val quiet : unit -> bool

val moderate : unit -> bool
