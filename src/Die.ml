(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

exception
  (* WARNING: ONLY USE IF Logging IS NOT AVAILABLE TO YOU FOR SOME REASON (e.g., inside Config) *)
    ExternalError of
    string

exception InternalError of string

exception UserError of string

type error = ExternalError | InternalError | UserError

let raise_error ?backtrace error ~msg =
  let do_raise exn =
    match backtrace with None -> raise exn | Some bt -> Caml.Printexc.raise_with_backtrace exn bt
  in
  match error with
  | ExternalError ->
      do_raise (ExternalError msg)
  | InternalError ->
      do_raise (InternalError msg)
  | UserError ->
      do_raise (UserError msg)


let die error fmt = F.kasprintf (fun msg -> raise_error error ~msg) fmt
