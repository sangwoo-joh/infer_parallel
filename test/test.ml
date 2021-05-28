open Infer_parallel
module L = Logging

let task_generator () : (int, string) ProcessPool.TaskGenerator.t =
  ProcessPool.TaskGenerator.of_list
    [1; 2; 3; 4; 5; 6; 7; 10; 19; 22; 22; 22; 22; 23; 13; 33; 23; 13; 23; 29; 31]


let meaningless_task : (int, string) Tasks.doer =
 fun number ->
  Unix.sleep number ;
  if Random.bool () then Some (Printf.sprintf "Did %d" number) else None


let meaninglessly_parallel () =
  let runner =
    let gc_stats_pre_fork = ref None in
    let child_prologue () = gc_stats_pre_fork := Some (GCStats.get ~since:ProgramStart) in
    let child_epilogue () =
      let gc_stats_in_fork =
        match !gc_stats_pre_fork with
        | Some stats ->
            Some (GCStats.get ~since:(Previous stats))
        | None ->
            L.internal_error "child process did not store GC stats in its prologue, what the hell?" ;
            None
      in
      gc_stats_in_fork
    in
    Tasks.Runner.create ~jobs:!Config.jobs ~f:meaningless_task ~child_prologue ~child_epilogue
      ~tasks:task_generator
  in
  let worker_stats = Tasks.Runner.run runner in
  let collected_stats =
    Core.Array.fold worker_stats ~init:[] ~f:(fun acc stat_opt ->
        match stat_opt with
        | None ->
            acc
        | Some gc_stat_opt ->
            Core.Option.fold ~init:acc ~f:(fun l x -> x :: l) gc_stat_opt)
  in
  collected_stats


let () = meaninglessly_parallel () |> ignore
