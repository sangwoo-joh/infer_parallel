open Infer_parallel
module L = Logging
module F = Format

(** Remember what the last status sent was so that we can update the status correctly when entering
    and exiting. In particular, we need to remember the original time. *)
let current_time = ref None

let update_taskbar work =
  let t0 = Mtime_clock.now () in
  let status = F.asprintf "I'm %d and I'm doing %s" (Unix.getpid ()) (string_of_int work) in
  !ProcessPoolState.update_status t0 status ;
  current_time := Some t0


let update_taskbar_done () =
  match !current_time with None -> () | Some t0 -> !ProcessPoolState.update_status t0 "I'm done!"


let task_generator () : (int, string) ProcessPool.TaskGenerator.t =
  ProcessPool.TaskGenerator.of_list [1; 1; 2; 2; 1; 2; 1; 1; 2; 2; 2; 1; 1; 2; 2; 1; 3]


let meaningless_task : (int, string) Tasks.doer =
 fun number ->
  update_taskbar number ;
  Unix.sleepf (Random.float 3.3) ;
  let result = if Random.bool () then Some (Printf.sprintf "Did %d" number) else None in
  update_taskbar_done () ;
  Unix.sleepf (Random.float 3.3) ;
  result


let meaninglessly_parallel () =
  (* Set some configs *)
  Config.jobs := 8 ;
  L.environment_info "Parallel jobs: %d@." !Config.jobs ;
  let runner =
    let gc_stats_pre_fork = ref None in
    let child_prologue () =
      gc_stats_pre_fork := Some (GCStats.get ~since:ProgramStart) ;
      Random.self_init ()
    in
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
