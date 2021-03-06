(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

include module type of Die

type style = Error | Fatal | Warning | Normal

val environment_info : ('a, F.formatter, unit) format -> 'a

val term_styles_of_style : style -> ANSITerminal.style list

val user_error : ('a, F.formatter, unit) format -> 'a

val user_warning : ('a, F.formatter, unit) format -> 'a

val internal_error : ('a, F.formatter, unit) format -> 'a

val external_error : ('a, F.formatter, unit) format -> 'a

val external_warning : ('a, F.formatter, unit) format -> 'a

type debug_level = Quiet | Moderate | Verbose

val debug : debug_level -> ('a, F.formatter, unit) format -> 'a
