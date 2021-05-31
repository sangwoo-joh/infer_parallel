(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val try_finally_swallow_timeout : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
(** Calls [f] then [finally] even if [f] raised an exception. The original exception is re-raised
    afterwards. Where possible use [SymOp.tri_finally] to avoid swallowing timeouts. *)

val with_file_in : string -> f:(In_channel.t -> 'a) -> 'a

val get_available_memory_MB : unit -> int option
(** On Linux systems, return [Some x] where [MemAvailable x] is in [/proc/meminfo]. Returns [None]
    in all other cases. *)

val num_cores : int
(** - On Linux, returns the number of physical cores (sockets * cores per socket).
    - On Darwin and Windows, returns half of the number of CPUs since most processors have 2
      hardware threads per core. *)

val set_best_cpu_for : int -> unit
(** Pins processes to CPUs aiming to saturate physical cores evenly. *)
