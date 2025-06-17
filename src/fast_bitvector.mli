(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

type t [@@deriving sexp]

val max_length : int

val create : len:int -> t
val create_full : len:int -> t

module type Ops := 
  sig
    type with_result
    val set : t -> int -> unit
    val clear : t -> int -> unit
    val set_to : t -> int -> bool -> unit

    val get : t -> int -> bool

    val equal : t -> t -> bool

    val not : t -> with_result
    val and_ : t -> t -> with_result
    val or_ : t -> t -> with_result
    val xor : t -> t -> with_result

    module Set : sig
      val mem : t -> int -> bool
      val intersect : t -> t -> with_result
      val complement : t -> with_result
      val symmetric_difference : t -> t -> with_result
      val difference : t -> t -> with_result
    end
  end

module Unsafe : Ops
  with type with_result := dst:t -> unit

include Ops
  with type with_result := dst:t -> unit
(** [dst] specifies the destination bitvector of the operation, for inplace operations,
    specify one of the operands as [dst] `*)

module Allocate : sig
  module Unsafe : Ops
    with type with_result := t

  include Ops
    with type with_result := t
end

module Bit_zero_first : sig
  type nonrec t = t [@@deriving sexp]
  val to_string : t -> string
  val of_string : string -> t
end

module Bit_zero_last : sig
  type nonrec t = t [@@deriving sexp]
  val to_string : t -> string
  val of_string : string -> t
end

val length : t -> int

val copy : t -> t
val append : t -> t -> t
val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
val map : t -> f:(bool -> bool) -> t

val popcount : t -> int

val set_all : t -> unit
val clear_all : t -> unit

val is_empty : t -> bool
val is_full : t -> bool

module Builder : sig
  type vector := t
  type t

  val create : unit -> t
  (** Create a new bitvector builder. *)

  val push : t -> bool -> unit
  (** Add a single bit to the bitvector (at the highest index). *)

  val to_bitvector : t -> vector
  (** Convert the builder to a bitvector. O(n). *)

  val reset : t -> unit
  (** Reset the builder, clearing all stored data *)

  val add_iter : t -> ((bool -> unit) -> unit) -> t
  (** Append an iterator to a builder. *)

  val add_seq : t -> bool Seq.t -> t
  (** Append a sequence to a builder. *)

  val of_iter : ((bool -> unit) -> unit) -> t
  (** Construct builder from an iterator. *)

  val of_seq : bool Seq.t -> t
  (** Construct build from a sequence. *)
end
