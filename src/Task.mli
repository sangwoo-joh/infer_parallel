(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Entry point for leveraging [TaskBar] and [ProcessPool]. *)

type ('work, 'result) command = 'work -> 'result option

val run_sequentially : f:('work, 'result) command -> 'work list -> (int, 'result option) Hashtbl.t
(** Run the tasks sequentially *)

(** A runner accepts new tasks repeatedly for parallel execution *)
module Runner : sig
  type ('work, 'final, 'result) t

  val create :
       jobs:int
    -> child_prologue:(unit -> unit)
    -> f:('work, 'result) command
    -> child_epilogue:(unit -> 'final)
    -> tasks:(unit -> ('work, 'result) ProcessPool.TaskManager.t)
    -> ('work, 'final, 'result) t
  (** Create a runner running [jobs] jobs in parallel *)

  val run : ('work, 'final, 'result) t -> 'final option Array.t * (int, 'result option) Hashtbl.t
  (** Start the given tasks with the runner and wait until completion *)
end
