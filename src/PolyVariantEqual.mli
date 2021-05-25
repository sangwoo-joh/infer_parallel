(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core

(** Open to bring equality [(=)] for polymorphic variants into scope *)

val ( = ) : ([> ] as 'a) -> 'a -> bool

val ( <> ) : ([> ] as 'a) -> 'a -> bool
