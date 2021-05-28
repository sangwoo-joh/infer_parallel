(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val oom_threshold : int option ref

val progress_bar : [`Multiline | `Plain | `Quiet] ref

val jobs : int ref

val keep_going : bool ref

type verbose_level = Quiet | Moderate | Verbose

val verbose_level : verbose_level ref

val verbose : unit -> bool

val quiet : unit -> bool

val moderate : unit -> bool
