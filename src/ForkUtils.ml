(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let protect ~f x =
  Epilogues.reset () ;
  Stdlib.flush_all () ;
  (* get different streams of random numbers in each fork, in particular to lessen contention in `Filename.mk_temp` *)
  Random.self_init () ;
  f x
