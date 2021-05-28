(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
include Die

type style = Error | Fatal | Warning | Normal

let term_styles_of_style = function
  | Error ->
      ANSITerminal.[Foreground Red]
  | Fatal ->
      ANSITerminal.[Bold; Foreground Red]
  | Warning ->
      ANSITerminal.[Foreground Yellow]
  | Normal ->
      ANSITerminal.[default]


let copy_formatter f =
  let out_string, flush = F.pp_get_formatter_output_functions f () in
  let out_funcs = F.pp_get_formatter_out_functions f () in
  let new_f = F.make_formatter out_string flush in
  F.pp_set_formatter_out_functions new_f out_funcs ;
  new_f


(** Returns a formatter that multiplexes to [fmt1] and [fmt2] *)
let dup_formatter fmt1 fmt2 =
  let out_funs1 = F.pp_get_formatter_out_functions fmt1 () in
  let out_funs2 = F.pp_get_formatter_out_functions fmt2 () in
  let f = copy_formatter fmt1 in
  F.pp_set_formatter_out_functions f
    { F.out_string=
        (fun s p n ->
          out_funs1.out_string s p n ;
          out_funs2.out_string s p n)
    ; out_indent=
        (fun n ->
          out_funs1.out_indent n ;
          out_funs2.out_indent n)
    ; out_flush=
        (fun () ->
          out_funs1.out_flush () ;
          out_funs2.out_flush ())
    ; out_newline=
        (fun () ->
          out_funs1.out_newline () ;
          out_funs2.out_newline ())
    ; out_spaces=
        (fun n ->
          out_funs1.out_spaces n ;
          out_funs2.out_spaces n) } ;
  f


type formatters =
  { file: F.formatter option  (** send to log file *)
  ; console_file: F.formatter  (** send both to console and log file *) }

let log_file = ref None

let logging_formatters = ref []

(* shared ref is less punishing to sloppy accounting of newlines *)
let is_newline = ref true

let prev_category = ref ""

let mk_file_formatter file_fmt category0 =
  let f = copy_formatter file_fmt in
  let out_functions_orig = F.pp_get_formatter_out_functions f () in
  let prefix = Printf.sprintf "[%d][%s] " (Pid.to_int (Unix.getpid ())) category0 in
  let print_prefix_if_newline () =
    let category_has_changed =
      (* take category + PID into account *)
      not (phys_equal !prev_category prefix)
    in
    if !is_newline || category_has_changed then (
      if (not !is_newline) && category_has_changed then
        (* category change but previous line has not ended: print newline *)
        out_functions_orig.out_newline () ;
      is_newline := false ;
      prev_category := prefix ;
      out_functions_orig.out_string prefix 0 (String.length prefix) )
  in
  let out_string s p n =
    print_prefix_if_newline () ;
    out_functions_orig.out_string s p n
  in
  let out_indent n =
    print_prefix_if_newline () ;
    out_functions_orig.out_indent n
  in
  let out_newline () =
    print_prefix_if_newline () ;
    out_functions_orig.out_newline () ;
    is_newline := true
  in
  let out_spaces n =
    print_prefix_if_newline () ;
    out_functions_orig.out_spaces n
  in
  F.pp_set_formatter_out_functions f
    {F.out_string; out_flush= out_functions_orig.out_flush; out_indent; out_newline; out_spaces} ;
  f


let color_console ?(use_stdout = false) scheme =
  let scheme = Option.value scheme ~default:Normal in
  let formatter = if use_stdout then F.std_formatter else F.err_formatter in
  let can_colorize = Unix.(isatty (if use_stdout then stdout else stderr)) in
  if can_colorize then (
    let styles = term_styles_of_style scheme in
    let orig_out_functions = F.pp_get_formatter_out_functions formatter () in
    let out_string s p n =
      let s = ANSITerminal.sprintf styles "%s" (String.slice s p n) in
      orig_out_functions.F.out_string s 0 (String.length s)
    in
    let out_newline () =
      (* Erase to end-of-line to avoid garbage, in particular when writing over the taskbar *)
      orig_out_functions.F.out_string TaskBar.erase_eol 0 (String.length TaskBar.erase_eol) ;
      orig_out_functions.F.out_newline ()
    in
    F.pp_set_formatter_out_functions formatter
      {(F.pp_get_formatter_out_functions formatter ()) with F.out_string; out_newline} ;
    formatter )
  else formatter


let register_formatter =
  let all_prefixes = ref [] in
  fun ?use_stdout ?color_scheme prefix ->
    all_prefixes := prefix :: !all_prefixes ;
    (* lazy so that we get a chance to register all prefixes before computing their max lenght for alignment purposes. *)
    lazy
      (let max_prefix = List.map ~f:String.length !all_prefixes |> List.fold_left ~f:max ~init:0 in
       let fill =
         let n = max_prefix - String.length prefix in
         String.make n ' '
       in
       let justified_prefix = fill ^ prefix in
       let mk_formatters () =
         let console = color_console ?use_stdout color_scheme in
         match !log_file with
         | Some (file_fmt, _) ->
             let file = mk_file_formatter file_fmt justified_prefix in
             let console_file = dup_formatter console file in
             {file= Some file; console_file}
         | None ->
             {file= None; console_file= console}
       in
       let formatters = mk_formatters () in
       let formatters_ref = ref formatters in
       logging_formatters := ((formatters_ref, mk_formatters), formatters) :: !logging_formatters ;
       formatters_ref)


let flush_formatters {file; console_file} =
  Option.iter file ~f:(fun file -> F.pp_print_flush file ()) ;
  F.pp_print_flush console_file ()


let close_logs () =
  let close_fmt (_, formatters) = flush_formatters formatters in
  List.iter ~f:close_fmt !logging_formatters ;
  Option.iter !log_file ~f:(function file_fmt, chan ->
      F.pp_print_flush file_fmt () ;
      Out_channel.close chan)


let () = Epilogues.register ~f:close_logs ~description:"Flushing logs and closing log files"

let log ~to_console ?(to_file = true) (lazy formatters) =
  match (to_console, to_file) with
  | false, false ->
      F.ifprintf F.std_formatter
  | true, _ when not (Config.quiet ()) ->
      F.fprintf !formatters.console_file
  | _ ->
      Option.value_map !formatters.file
        ~f:(fun file_fmt -> F.fprintf file_fmt)
        ~default:(F.fprintf F.err_formatter)


let user_error_file_fmts = register_formatter ~color_scheme:Fatal "user error"

let user_error fmt = log ~to_console:true user_error_file_fmts fmt

let user_warning_file_fmts = register_formatter ~color_scheme:Warning "user warning"

let user_warning fmt = log ~to_console:(not (Config.quiet ())) user_warning_file_fmts fmt

let internal_error_file_fmts = register_formatter ~color_scheme:Error "internal error"

let internal_error fmt = log ~to_console:true internal_error_file_fmts fmt

let external_error_file_fmts = register_formatter ~color_scheme:Error "external error"

let external_error fmt = log ~to_console:true external_error_file_fmts fmt

let external_warning_file_fmts = register_formatter ~color_scheme:Warning "external warning"

let external_warning fmt = log ~to_console:true external_warning_file_fmts fmt

let die error msg =
  let log_of_kind error fmt =
    match error with
    | UserError ->
        log ~to_console:false user_error_file_fmts fmt
    | ExternalError ->
        log ~to_console:false external_error_file_fmts fmt
    | InternalError ->
        log ~to_console:false internal_error_file_fmts fmt
  in
  let backtrace = Caml.Printexc.get_raw_backtrace () in
  F.kasprintf
    (fun msg ->
      log_of_kind error "%s@\n%s@." msg (Caml.Printexc.raw_backtrace_to_string backtrace) ;
      raise_error ~backtrace error ~msg)
    msg
