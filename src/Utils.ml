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

(** Recursively traverse a path for files ending with a given extension *)
let find_files ~path ~extension =
  let rec traverse_dir_aux init dir_path =
    let aux base_path files rel_path =
      let full_path = base_path ^/ rel_path in
      match Unix.(stat full_path).st_kind with
      | Unix.S_REG when String.is_suffix ~suffix:extension full_path ->
          full_path :: files
      | Unix.S_DIR ->
          traverse_dir_aux files full_path
      | _ | (exception Unix.Unix_error (ENOENT, _, _)) ->
          files
    in
    Sys.fold_dir ~init ~f:(aux dir_path) dir_path
  in
  traverse_dir_aux [] path


(** Recursively traverse a path for folders, returning results by a given fold function *)
let fold_folders ~init ~f ~path =
  let rec traverse_dir_aux acc dir_path =
    let aux base_path acc' rel_path =
      let full_path = base_path ^/ rel_path in
      match Unix.(stat full_path).st_kind with
      | Unix.S_DIR ->
          traverse_dir_aux (f acc' full_path) full_path
      | _ | (exception Unix.Unix_error (ENOENT, _, _)) ->
          acc'
    in
    Sys.fold_dir ~init:acc ~f:(aux dir_path) dir_path
  in
  traverse_dir_aux init path


(** Compute a 32-character hexadecimal crc using the Digest module *)
let string_crc_hex32 s = Caml.Digest.to_hex (Caml.Digest.string s)

(** Read a source file and return a list of lines *)
let read_file fname =
  let res = ref [] in
  let cin_ref = ref None in
  let cleanup () = match !cin_ref with None -> () | Some cin -> In_channel.close cin in
  try
    let cin = In_channel.create fname in
    cin_ref := Some cin ;
    while true do
      let line = In_channel.input_line_exn cin in
      res := line :: !res
    done ;
    assert false
  with
  | End_of_file ->
      cleanup () ;
      Ok (List.rev !res)
  | Sys_error err ->
      cleanup () ;
      Error err


(** [normalize_path_from ~root path] removes ".." and "." parts of [root/path] when possible and
    returns the new [root] and [path]. e.g., if [root = "r"] and [path="a/../../../foo/./bar"] then
    the result is [("../foo/bar", ".")]) (in particular "r/a/../../../foo/./bar" and "./../foo/bar"
    represent the same file)*)
let normalize_path_from ~root fname =
  let add_entry (rev_done, rev_root) entry =
    match (entry, rev_done, rev_root) with
    | ".", _, _ ->
        (* path/. --> path *)
        (rev_done, rev_root)
    | "..", [], ["/"] | "..", ["/"], _ ->
        (* /.. -> / *)
        (rev_done, rev_root)
    | "..", [], ("." | "..") :: _ | "..", ("." | "..") :: _, _ ->
        (* id on {., ..}/.. *)
        (entry :: rev_done, rev_root)
    | "..", [], _ :: rev_root_parent ->
        (* eat from the root part if it's not / *)
        ([], rev_root_parent)
    | "..", _ :: rev_done_parent, _ ->
        (* path/dir/.. -> path *)
        (rev_done_parent, rev_root)
    | _ ->
        (entry :: rev_done, rev_root)
  in
  let rev_root =
    (* Remove the leading "." inserted by [Filename.parts] on relative paths. We don't need to do that for [Filename.parts fname] because the "." will go away during normalization in [add_entry] *)
    let root_without_leading_dot =
      match Filename.parts root with "." :: (_ :: _ as rest) -> rest | parts -> parts
    in
    List.rev root_without_leading_dot
  in
  let rev_result, rev_root = Filename.parts fname |> List.fold ~init:([], rev_root) ~f:add_entry in
  (* Don't use [Filename.of_parts] because it doesn't like empty lists and produces relative paths "./like/this" instead of "like/this" *)
  let filename_of_rev_parts = function
    | [] ->
        "."
    | _ :: _ as rev_parts ->
        let parts = List.rev rev_parts in
        if String.equal (List.hd_exn parts) "/" then
          "/" ^ String.concat ~sep:Filename.dir_sep (List.tl_exn parts)
        else String.concat ~sep:Filename.dir_sep parts
  in
  (filename_of_rev_parts rev_result, filename_of_rev_parts rev_root)


(** Normalize a path without a root *)
let normalize_path fname = fname |> normalize_path_from ~root:"." |> fst

(** Convert a filename to an absolute one if it is relative, and normalize "." and ".." *)
let filename_to_absolute ~root fname =
  let abs_fname = if Filename.is_absolute fname then fname else root ^/ fname in
  normalize_path_from ~root:"/" abs_fname |> fst


(** Convert an absolute filename to one relative to a root directoy. Returns [None] if filename is
    not under root. The backtrack level sets the maximum level of steps in the parent directories to
    search for a common prefix. *)
let filename_to_relative ?(force_full_backtrack = false) ?(backtrack = 0) ~root fname =
  let rec relativize_if_under origin target =
    match (origin, target) with
    | x :: xs, y :: ys when String.equal x y ->
        relativize_if_under xs ys
    | _ :: _, _ when force_full_backtrack || backtrack >= List.length origin ->
        let parent_dir = List.init (List.length origin) ~f:(fun _ -> Filename.parent_dir_name) in
        Some (Filename.of_parts (parent_dir @ target))
    | [], [] ->
        Some "."
    | [], ys ->
        Some (Filename.of_parts ys)
    | _ ->
        None
  in
  relativize_if_under (Filename.parts root) (Filename.parts fname)


(** Type for files used for printing *)
type outfile = {fname: string; out_c: Out_channel.t; fmt: Format.formatter}

(** Create an outfile for the command line*)
let create_outfile fname =
  try
    let out_c = Out_channel.create fname in
    let fmt = F.formatter_of_out_channel out_c in
    Some {fname; out_c; fmt}
  with Sys_error err ->
    F.fprintf F.err_formatter "error: cannot create file %s: %s@." fname err ;
    None


let close_outfile outf = Out_channel.close outf.out_c

(** Functional fold function over all the file of a directory *)
let directory_fold ~f ~init path =
  let collect current_dir (acc, dirs) path =
    let full_path = current_dir ^/ path in
    try
      if Sys.is_directory full_path = `Yes then (acc, full_path :: dirs) else (f acc full_path, dirs)
    with Sys_error _ -> (acc, dirs)
  in
  let rec loop acc dirs =
    match dirs with
    | [] ->
        acc
    | d :: tl ->
        let acc', dirs' = Array.fold ~f:(collect d) ~init:(acc, tl) (Sys.readdir d) in
        loop acc' dirs'
  in
  if Sys.is_directory path = `Yes then loop init [path] else f init path


(** Functional iter function over all the file of a directory *)
let directory_iter ~f path =
  let apply current_dir dirs path =
    let full_path = current_dir ^/ path in
    try
      if Sys.is_directory full_path = `Yes then full_path :: dirs
      else (
        f full_path ;
        dirs )
    with Sys_error _ -> dirs
  in
  let rec loop dirs =
    match dirs with
    | [] ->
        ()
    | d :: tl ->
        let dirs' = Array.fold ~f:(apply d) ~init:tl (Sys.readdir d) in
        loop dirs'
  in
  if Sys.is_directory path = `Yes then loop [path] else f path


let read_json_file path =
  try Ok (Yojson.Basic.from_file path) with Sys_error err | Yojson.Json_error err -> Error err


let do_finally_swallow_timeout ~f ~finally =
  let res =
    try f ()
    with exc ->
      IExn.reraise_after exc ~f:(fun () ->
          try finally () |> ignore with _ -> (* swallow in favor of the original exception *) ())
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


let with_file_out file ~f =
  let out_c = Out_channel.create file in
  let f () = f out_c in
  let finally () = Out_channel.close out_c in
  try_finally_swallow_timeout ~f ~finally


(** Like [with_file_out] but uses a fresh intermediate temporary file and rename to vaoid
    write-write races *)
let with_intermediate_temp_file_out file ~f =
  let temp_file, temp_outc = Filename.open_temp_file ~in_dir:(Filename.dirname file) "infer" "" in
  let f () = f temp_outc in
  let finally () =
    Out_channel.close temp_outc ;
    Unix.rename ~src:temp_file ~dst:file
  in
  try_finally_swallow_timeout ~f ~finally


let write_json_to_file destfile json =
  with_file_out destfile ~f:(fun out_c -> Yojson.Basic.pretty_to_channel out_c json)


let write_channel_in ~f in_c =
  try
    while true do
      f @@ In_channel.input_line_exn in_c
    done
  with End_of_file -> ()


(** Consume and ignore all the lines from the channel until [End_of_file] is reached *)
let consume_in in_c = write_channel_in ~f:ignore in_c

(** Echo the lines we get to stdout until [End_of_file] is reached *)
let echo_in in_c = write_channel_in ~f:print_endline in_c

let write_process_in command read =
  let chan = Unix.open_process_in command in
  let f () = read chan in
  let finally () =
    consume_in chan ;
    Unix.close_process_in chan
  in
  do_finally_swallow_timeout ~f ~finally


let is_dir_kind (kind : Unix.file_kind) = match kind with S_DIR -> true | _ -> false

(** Recursively create a directory if it does not exist *)
let create_dir dir =
  try
    if not (is_dir_kind Unix.(stat dir).st_kind) then
      L.(die ExternalError) "file '%s' already exists and is not a directory" dir
  with Unix.Unix_error _ -> (
    try Unix.mkdir_p dir ~perm:0o700
    with Unix.Unix_error _ ->
      let created_concurrently =
        (* check if another process created it meanwhile *)
        try Poly.equal Unix.(stat dir).st_kind Unix.S_DIR with Unix.Unix_error _ -> false
      in
      if not created_concurrently then L.(die ExternalError) "cannot created directory '%s'" dir )


(** Create an out channel with creating missing directories *)
let out_channel_create_with_dir fname =
  try Out_channel.create fname
  with Sys_error _ ->
    Unix.mkdir_p ~perm:0o700 (Filename.dirname fname) ;
    Out_channel.create fname


let realpath_cache = Hashtbl.create 1023

(** [realpath warn_on_error path] returns path with all symbolic links resolved. It caches results
    of previous calls to avoid expensive system calls. WARNING: If [warn_or_error] is false, no
    warning will be shown whenever an error occurs for the given path (e.g. if it does not exist) *)
let realpath ?(warn_on_error = true) path =
  match Hashtbl.find realpath_cache path with
  | exception Caml.Not_found -> (
    match Filename.realpath path with
    | realpath ->
        Hashtbl.add realpath_cache path (Ok realpath) ;
        realpath
    | exception (Unix.Unix_error (code, _, arg) as exn) ->
        IExn.reraise_after exn ~f:(fun () ->
            if warn_on_error then
              F.eprintf "WARNING: Failed to resolve file %s with \"%s\" @\n@." arg
                (Unix.Error.message code) ;
            (* cache failure as well *)
            Hashtbl.add realpath_cache path (Error exn)) )
  | Ok path ->
      path
  | Error exn ->
      raise exn


let devnull = lazy (Unix.openfile "/dev/null" ~mode:[Unix.O_WRONLY])

(** Wraps a function expecting 2 arguments in another that temporarily redirects stderr to /dev/null
    for the duration of the function call *)
let suppress_stderr2 f2 x1 x2 =
  let restore_stderr src =
    Unix.dup2 ~src ~dst:Unix.stderr () ;
    Unix.close src
  in
  let orig_stderr = Unix.dup Unix.stderr in
  Unix.dup2 ~src:(Lazy.force devnull) ~dst:Unix.stderr () ;
  let f () = f2 x1 x2 in
  let finally () = restore_stderr orig_stderr in
  protect ~f ~finally


(** [rmtree path] removes [path] and if [path] is a directory, recursively removes its contents *)
let rec rmtree name =
  match Unix.(lstat name).st_kind with
  | S_DIR ->
      let dir = Unix.opendir name in
      let rec rmdir dir =
        match Unix.readdir_opt dir with
        | Some entry ->
            if
              not (List.exists ~f:(String.equal entry) Filename.[current_dir_name; parent_dir_name])
            then rmtree (name ^/ entry) ;
            rmdir dir
        | None ->
            Unix.closedir dir ;
            Unix.rmdir name
      in
      rmdir dir
  | _ ->
      Unix.unlink name
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      ()


(** [Hashtbl.hash] only hashes the first 10 meaningful values, [better_hash] uses everything. *)
let better_hash x = Marshal.to_string x [Marshal.No_sharing] |> Caml.Digest.string

let unlink_file_on_exit temp_file =
  let description = "Cleaning temporary file " ^ temp_file in
  Epilogues.register ~description ~f:(fun () -> try Unix.unlink temp_file with _ -> ())


(** Drop at most one layer of well-balanced first and last characters satisfying [drop] from the
    string. E.g., [strip_balanced_once ~drop:(function | 'a' | 'x' -> true | _ -> false) "xaabax"]
    returns "aaba". *)
let strip_balanced_once ~drop s =
  let n = String.length s in
  if n < 2 then s
  else
    let first = String.unsafe_get s 0 in
    if Char.equal first (String.unsafe_get s (n - 1)) && drop first then String.slice s 1 (n - 1)
    else s


let die_expected_yojson_type expected yojson_obj ~src ~example =
  let eg = if String.equal example "" then "" else " (e.g. '" ^ example ^ "')" in
  L.(die UserError)
    "in %s expected json %s%s not %s" src expected eg
    (Yojson.Basic.to_string yojson_obj)


(** Verify a json object (or empty list) and return the corresponding assoc list. Otherwise die with
    a message including [src]. *)
let assoc_of_yojson yojson_obj ~src =
  match yojson_obj with
  | `Assoc assoc ->
      assoc
  | `List [] ->
      []
  | _ ->
      die_expected_yojson_type "object" yojson_obj ~example:{|{ "foo": "bar" }|} ~src


(** Verify a json string and return the corresponding OCaml string. Otherwise die with a message
    including [src]. *)
let string_of_yojson yojson_obj ~src =
  match yojson_obj with
  | `String str ->
      str
  | _ ->
      die_expected_yojson_type "string" yojson_obj ~example:{|"foo"|} ~src


(** Verify a json list of strings and return the corresponding OCaml string list. Otherwise die with
    a message including [src]. *)
let string_list_of_yojson yojson_obj ~src =
  let list_of_yojson yojson_obj ~src =
    match yojson_obj with
    | `List l ->
        l
    | _ ->
        die_expected_yojson_type "list" yojson_obj ~example:{|["foo", "bar"]|} ~src
  in
  List.map ~f:(string_of_yojson ~src) (list_of_yojson yojson_obj ~src)


let yojson_lookup yojson_assoc elt_name ~src ~f ~default =
  let src = src ^ " -> " ^ elt_name in
  Option.value_map ~default
    ~f:(fun res -> f res ~src)
    (List.Assoc.find ~equal:String.equal yojson_assoc elt_name)


(** Returns the execution time of [f] in milliseconds together with its result *)
let timeit ~f =
  let start_time = Mtime_clock.counter () in
  let ret = f () in
  let duration_ms = Mtime_clock.count start_time |> Mtime.Span.to_ms |> int_of_float in
  (ret, duration_ms)


let do_in_dir ~dir ~f =
  let cwd = Unix.getcwd () in
  Unix.chdir dir ;
  let popd () = Unix.chdir cwd in
  try_finally_swallow_timeout ~f ~finally:popd


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
        sockets * cores_per_socket)
  in
  (* NOTE: check build platform just using Sys.os_type *)
  match Sys.os_type with "Unix" -> physical_cores () | _ -> cpus / 2


(** Pins processes to CPUs aiming to saturate physical cores evenly. *)
let set_best_cpu_for worker_id =
  let threads_per_core = cpus / num_cores in
  let chosen_core = worker_id * threads_per_core % num_cores in
  let chosne_thread_in_core = worker_id * threads_per_core / num_cores in
  Setcore.setcore ((chosen_core * threads_per_core) + chosne_thread_in_core)


(** Fold over each filename in the given [zip_filename]. *)
let zip_fold_filenames ~init ~f ~zip_filename =
  let file_in = Zip.open_in zip_filename in
  let collect acc (entry : Zip.entry) = f acc entry.filename in
  let result = List.fold ~f:collect ~init (Zip.entries file_in) in
  Zip.close_in file_in ;
  result
