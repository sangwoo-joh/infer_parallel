(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type ('a, 'b) command = 'a -> 'b option

module Runner = struct
  type ('work, 'final, 'result) t = ('work, 'final, 'result) ProcessPool.t

  let create ~jobs ~child_prologue ~f ~child_epilogue ~tasks =
    ProcessPool.create ~jobs ~f ~child_epilogue ~tasks
      ~child_prologue:
        ((* hack: we'll continue executing after the function passed
            to [protect], despite what the name might suggest *)
         ForkUtils.protect ~f:child_prologue )


  let run runner =
    (* Flush here all buffers to avoid passing unflushed data to
       forked processes, leading to duplication *)
    Stdlib.flush_all () ;
    (* Compact heap before forking *)
    Gc.compact () ;
    ProcessPool.run runner
end

let run_sequentially ~(f : ('a, 'b) command) (tasks : 'a list) : unit =
  let task_generator = ProcessPool.TaskGenerator.of_list tasks in
  let task_bar = TaskBar.create ~jobs:1 in
  (ProcessState.update_status :=
     fun t status ->
       TaskBar.update_status task_bar ~slot:0 t status ;
       TaskBar.refresh task_bar ) ;
  TaskBar.set_tasks_total task_bar (task_generator.remaining_tasks ()) ;
  TaskBar.tasks_done_reset task_bar ;
  let rec run_tasks () =
    if not (task_generator.is_empty ()) then (
      Option.iter (task_generator.next ()) ~f:(fun t ->
          let result = f t in
          task_generator.finished ~result t ) ;
      TaskBar.set_remaining_tasks task_bar (task_generator.remaining_tasks ()) ;
      TaskBar.refresh task_bar ;
      run_tasks () )
  in
  run_tasks () ;
  TaskBar.finish task_bar