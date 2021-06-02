open Infer_parallel
module L = Logging
module F = Format
module P = Printf

(** Remember what the last status sent was so that we can update the status correctly when entering
    and exiting. In particular, we need to remember the original time. *)
let current_time = ref None

let update_taskbar work =
  let t0 = Mtime_clock.now () in
  let status = F.asprintf "I'm %d and I'm doing %s" (Unix.getpid ()) (string_of_int work) in
  !ProcessState.update_status t0 status ;
  current_time := Some t0


let update_taskbar_done () =
  match !current_time with None -> () | Some t0 -> !ProcessState.update_status t0 "I'm done!"


let task_manager () : (int, int) ProcessPool.TaskManager.t =
  ProcessPool.TaskManager.of_list [1; 1; 2; 2; 1; 2; 1; 1; 2; 2; 2; 1; 1; 2; 2; 1; 3]


let meaningless_task : (int, int) Task.command =
 fun number ->
  update_taskbar number ;
  Unix.sleepf (Random.float 3.3) ;
  let result = if Random.bool () then Some number else None in
  update_taskbar_done () ;
  Unix.sleepf (Random.float 3.3) ;
  result


let meaninglessly_parallel () =
  (* Set some configs *)
  Config.jobs := 8 ;
  Config.progress_bar := `Quiet ;
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
    Task.Runner.create ~jobs:!Config.jobs ~f:meaningless_task ~child_prologue ~child_epilogue
      ~tasks:task_manager
  in
  let final_array, result_table = Task.Runner.run runner in
  let collected_stats =
    Core.Array.fold final_array ~init:[] ~f:(fun acc stat_opt ->
        match stat_opt with
        | None ->
            acc
        | Some gc_stat_opt ->
            Core.Option.fold ~init:acc ~f:(fun l x -> x :: l) gc_stat_opt )
  in
  (* Show results *)
  L.user_warning "Total size of collected final: %d\n" (List.length collected_stats) ;
  L.user_warning "Total size of result: %d\n" (Core.Hashtbl.length result_table) ;
  Core.Hashtbl.iteri result_table ~f:(fun ~key ~data ->
      match data with
      | None ->
          L.user_warning "%dth task is dead\n" key
      | Some number ->
          L.user_warning "%dth task %d survived!\n" key number ) ;
  let survived =
    Core.Hashtbl.fold result_table ~init:0 ~f:(fun ~key:_ ~data acc ->
        match data with None -> acc | Some _ -> acc + 1 )
  in
  L.user_warning "Total %d task was survived\n" survived


let () = meaninglessly_parallel () |> ignore
