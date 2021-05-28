(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let oom_threshold : int option ref = ref None

let progress_bar : [`Multiline | `Plain | `Quiet] ref = ref `Multiline

let jobs : int ref = ref Utils.num_cores

let keep_going : bool ref = ref true

type verbose_level = Quiet | Moderate | Verbose

let verbose_level = ref Verbose

let verbose () = match !verbose_level with Verbose -> true | _ -> false

let quiet () = match !verbose_level with Quiet -> true | _ -> false

let moderate () = match !verbose_level with Moderate -> true | _ -> false
