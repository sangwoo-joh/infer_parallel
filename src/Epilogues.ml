(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let early_callback = ref (fun () -> ())

let late_callback = ref (fun () -> ())

let register callback_ref ~f ~description =
  let f_with_no_exception () =
    try f ()
    with exn ->
      F.eprintf {|%a: Error while running epilogue "%s":@ %a. @ Powering through...@.|} Pid.pp
        (Unix.getpid ()) description Exn.pp exn
  in
  let g = !callback_ref in
  callback_ref :=
    fun () ->
      f_with_no_exception () ;
      g ()


let register_early ~f ~description = register early_callback ~f ~description

let register_late ~f ~description = register late_callback ~f ~description

let early () = !early_callback ()

let late () = !late_callback ()

let run () =
  early () ;
  late ()


(** Raised when we are interrupted by SIGINT (Ctrl-C). *)
exception Sigint

let () = Caml.Sys.(set_signal sigint (Signal_handle (fun _ -> raise Sigint)))

let reset () =
  (early_callback := fun () -> ()) ;
  late_callback := fun () -> ()


let register = register_early
