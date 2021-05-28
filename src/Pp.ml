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

let equal_color = [%compare.equal: color]

(** Map subexpressions (as [Obj.t] element compared by physical equality) to colors *)
type colormap = Obj.t -> color

type simple_kind = Default | WithType

type print_kind = Text | Html [@@deriving compare]

let equal_print_kind = [%compare.equal: print_kind]

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

let reset_obj_sub pe = {pe with obj_sub= None}

let set_obj_sub pe (sub : 'a -> 'a) =
  let new_obj_sub x =
    let x' = Obj.repr (sub (Obj.obj x)) in
    match pe.obj_sub with None -> x' | Some sub' -> sub' x'
  in
  {pe with obj_sub= Some new_obj_sub}


let extend_colormap pe (x : Obj.t) (c : color) =
  let colormap (y : Obj.t) = if phys_equal x y then c else pe.cmap_norm y in
  {pe with cmap_norm= colormap}


let color_string = function
  | Black ->
      "color_black"
  | Blue ->
      "color_blue"
  | Green ->
      "color_green"
  | Orange ->
      "color_orange"
  | Red ->
      "color_red"


let colormap_black (_ : Obj.t) = Black

let colormap_red (_ : Obj.t) = Red

let colormap_from_color color (_ : Obj.t) = color

let html_with_color color pp f x =
  F.fprintf f "<span class='%s'>%a</span>" (color_string color) pp x


let color_wrapper pe ppf x ~f =
  match pe.kind with
  | Html when not (equal_color (pe.cmap_norm (Obj.repr x)) pe.color) ->
      let color = pe.cmap_norm (Obj.repr x) in
      let pe' =
        if equal_color color Red then {pe with cmap_norm= colormap_red; color= Red}
        else {pe with color}
      in
      html_with_color color (f pe') ppf x
  | _ ->
      f pe ppf x


let text =
  { opt= Default
  ; kind= Text
  ; break_lines= false
  ; cmap_norm= colormap_black
  ; cmap_foot= colormap_black
  ; color= Black
  ; obj_sub= None }


let text_break = {text with break_lines= true}

let html color =
  { text with
    kind= Html
  ; cmap_norm= colormap_from_color color
  ; cmap_foot= colormap_from_color color
  ; color }


let seq ?(print_env = text) ?sep:(sep_text = " ") ?(sep_html = sep_text) pp =
  let rec pp_aux f = function
    | [] ->
        ()
    | [x] ->
        pp f x
    | x :: tl ->
        let sep = match print_env.kind with Text -> sep_text | Html -> sep_html in
        if print_env.break_lines then F.fprintf f "%a%s@ %a" pp x sep pp_aux tl
        else F.fprintf f "%a%s%a" pp x sep pp_aux tl
  in
  pp_aux


let comma_seq ?print_env pp f l = seq ?print_env ~sep:"," pp f l

let semicolon_seq ?print_env pp f l = seq ?print_env ~sep:";" pp f l

let comma_seq_diff pp pe0 f =
  let rec doit = function
    | [] ->
        ()
    | [x] ->
        color_wrapper pe0 f x ~f:(fun _ -> pp)
    | x :: tl ->
        color_wrapper pe0 f x ~f:(fun _ -> pp) ;
        F.pp_print_string f ", " ;
        doit tl
  in
  doit


let pair ~fst ~snd fmt (a, b) = F.fprintf fmt "(%a,@,%a)" fst a snd b

let in_backticks pp fmt x = F.fprintf fmt "`%a`" pp x

let collection :
       fold:('t, 'item, bool) Container.fold
    -> sep:string
    -> pp_item:(F.formatter -> 'item -> unit)
    -> F.formatter
    -> 't
    -> unit =
 fun ~fold ~sep ~pp_item fmt col ->
  let pp_col_aux is_first item =
    F.fprintf fmt "@[<h>%s%a@]" (if is_first then "" else sep) pp_item item ;
    (* [is_first] not true anymore*) false
  in
  F.fprintf fmt "@[<hv>%t@]" (fun _ -> fold col ~init:true ~f:pp_col_aux |> ignore)
