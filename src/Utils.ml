(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
module F = Format
module L = Die
module Hashtbl = Caml.Hashtbl

let do_finally_swallow_timeout ~f ~finally =
  let res =
    try f ()
    with exc ->
      IExn.reraise_after exc ~f:(fun () ->
          try finally () |> ignore with _ -> (* swallow in favor of the original exception *) () )
  in
  let res' = finally () in
  (res, res')


(** Calls [f] then [finally] even if [f] raised an exception. The original exception is re-raised
    afterwards. Where possible use [SymOp.tri_finally] to avoid swallowing timeouts. *)
let try_finally_swallow_timeout ~f ~finally =
  let res, () = do_finally_swallow_timeout ~f ~finally in
  res


let with_file_in file ~f =
  let in_c = In_channel.create file in
  let f () = f in_c in
  let finally () = In_channel.close in_c in
  try_finally_swallow_timeout ~f ~finally


let get_available_memory_MB () =
  let proc_meminfo = "/proc/meminfo" in
  let rec scan_for_expected_output in_channel =
    match In_channel.input_line in_channel with
    | None ->
        None
    | Some line -> (
      try Scanf.sscanf line "MemAvailable: %u kB" (fun mem_kB -> Some (mem_kB / 1024))
      with Scanf.Scan_failure _ -> scan_for_expected_output in_channel )
  in
  if Sys.file_exists proc_meminfo <> `Yes then None
  else with_file_in proc_meminfo ~f:scan_for_expected_output


let cpus = Setcore.numcores ()

let num_cores =
  let physical_cores () =
    with_file_in "/proc/cpuinfo" ~f:(fun in_c ->
        let physical_or_core_regexp =
          Re.Str.regexp "\\(physical id\\|core id\\)[^0-9]+\\([0-9]+\\).*"
        in
        let rec loop sockets cores =
          match In_channel.input_line ~fix_win_eol:true in_c with
          | None ->
              (Int.Set.length sockets, Int.Set.length cores)
          | Some line when Re.Str.string_match physical_or_core_regexp line 0 -> (
              let value = Re.Str.matched_group 2 line |> int_of_string in
              match Re.Str.matched_group 1 line with
              | "physical id" ->
                  loop (Int.Set.add sockets value) cores
              | "core id" ->
                  loop sockets (Int.Set.add cores value)
              | _ ->
                  L.(die InternalError) "Counldn't parse line '%s' from /proc/cpuinfo." line )
          | Some _ ->
              loop sockets cores
        in
        let sockets, cores_per_socket = loop Int.Set.empty Int.Set.empty in
        sockets * cores_per_socket )
  in
  (* NOTE: check build platform just using Sys.os_type *)
  match Sys.os_type with "Unix" -> physical_cores () | _ -> cpus / 2


(** Pins processes to CPUs aiming to saturate physical cores evenly. *)
let set_best_cpu_for worker_id =
  let threads_per_core = cpus / num_cores in
  let chosen_core = worker_id * threads_per_core % num_cores in
  let chosne_thread_in_core = worker_id * threads_per_core / num_cores in
  Setcore.setcore ((chosen_core * threads_per_core) + chosne_thread_in_core)
