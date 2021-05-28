(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let pp_collection ~pp_item fmt col =
  let f prev_opt item =
    prev_opt |> Option.iter ~f:(F.fprintf fmt "@[<h>%a,@]@ " pp_item) ;
    Some item
  in
  let pp_aux fmt col =
    List.fold col ~init:None ~f |> Option.iter ~f:(F.fprintf fmt "@[<h>%a@]" pp_item)
  in
  F.fprintf fmt "@[<hv 2>{ %a}@]" pp_aux col


module type Printable = sig
  type t

  val pp : F.formatter -> t -> unit
end

module type PrintableEquatable = sig
  include Printable

  val equal : t -> t -> bool
end

module type PrintableOrdered = sig
  include Caml.Set.OrderedType

  include Printable with type t := t
end

module type PrintableEquatableOrdered = sig
  include Caml.Set.OrderedType

  include PrintableEquatable with type t := t
end

module type PpSet = sig
  include Caml.Set.S

  include Printable with type t := t

  val pp_element : F.formatter -> elt -> unit
end

module type PpMap = sig
  include Caml.Map.S

  val pp_key : F.formatter -> key -> unit

  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

module MakePpSet (Ord : PrintableOrdered) = struct
  include Caml.Set.Make (Ord)

  let pp_element = Ord.pp

  let pp fmt t = pp_collection ~pp_item:pp_element fmt (elements t)
end

module MakePpMap (Ord : PrintableOrdered) = struct
  include Caml.Map.Make (Ord)

  let pp_key = Ord.pp

  let pp ~pp_value fmt t =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Ord.pp k pp_value v in
    pp_collection ~pp_item fmt (bindings t)
end
