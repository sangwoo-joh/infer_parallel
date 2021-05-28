(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * Modified work Copyright (c) Sangwoo Joh
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Wrappers for making pretty-printable modules *)

val pp_collection : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit

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

module MakePpSet (Ord : PrintableOrdered) : PpSet with type elt = Ord.t

module MakePpMap (Ord : PrintableOrdered) : PpMap with type key = Ord.t
