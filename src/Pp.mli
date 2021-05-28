(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

type color = Black | Blue | Green | Orange | Red [@@deriving compare]

val equal_color : color -> color -> bool

(** Map subexpressions (as [Obj.t] element compared by physical equality) to colors *)
type colormap = Obj.t -> color

type simple_kind = Default | WithType

type print_kind = Text | Html [@@deriving compare]

val equal_print_kind : print_kind -> print_kind -> bool

(** Print environment threaded through all the printing functions *)
type env =
  { opt: simple_kind
  ; kind: print_kind
  ; break_lines: bool
        (** whether to let [Format] add its own line breaks or not (false by default)*)
  ; cmap_norm: colormap
  ; cmap_foot: colormap
  ; color: color
  ; obj_sub: (Obj.t -> Obj.t) option  (** generic object substitution *) }

val reset_obj_sub : env -> env

val set_obj_sub : env -> ('a -> 'a) -> env
(** Set the object substitution, which is supposed to preserve the type. *)

val extend_colormap : env -> Obj.t -> color -> env

val color_wrapper : env -> F.formatter -> 'a -> f:(env -> F.formatter -> 'a -> unit) -> unit

val text : env

val text_break : env

val html : color -> env

val color_string : color -> string

val html_with_color : color -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit

val seq :
     ?print_env:env
  -> ?sep:string
  -> ?sep_html:string
  -> (F.formatter -> 'a -> unit)
  -> F.formatter
  -> 'a list
  -> unit
(** Pretty print a sequencewith [sep] followed by a space between each element. By default,
    [print_env] is [text] and [sep] is "" and [sep_html] set to [sep]. *)

val comma_seq : ?print_env:env -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
(** Pretty print a comma-separated sequence *)

val comma_seq_diff : (F.formatter -> 'a -> unit) -> env -> F.formatter -> 'a list -> unit

val semicolon_seq : ?print_env:env -> (F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit

val pair :
     fst:(F.formatter -> 'a -> unit)
  -> snd:(F.formatter -> 'b -> unit)
  -> F.formatter
  -> 'a * 'b
  -> unit

val in_backticks : (F.formatter -> 'a -> unit) -> F.formatter -> 'a -> unit [@@warning "-32"]

val collection :
     fold:('t, 'item, bool) Container.fold
  -> sep:string
  -> pp_item:(F.formatter -> 'item -> unit)
  -> F.formatter
  -> 't
  -> unit
