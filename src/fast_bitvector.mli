(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

type t [@@deriving sexp]

val max_length : int
(** Maximum length of any bitvector. Depends on host architecture. *)

val create : len:int -> t
(** Create a bitvector filled with zeroes of length [len]. *)

val create_full : len:int -> t
(** Create a bitvector filled with ones of size [len]. *)

module type Ops := 
  sig
    type with_result
    val set : t -> int -> unit
    val clear : t -> int -> unit
    val set_to : t -> int -> bool -> unit

    val get : t -> int -> bool

    val equal : t -> t -> bool
    (** Compare two bitvectors for equality. *)

    val equal_modulo : modulo:t -> t -> t -> bool
    (** Test whether two bitvectors are equal in the positions set in [modulo] *)

    val not : t -> with_result
    val and_ : t -> t -> with_result
    val nand : t -> t -> with_result
    val or_ : t -> t -> with_result
    val nor : t -> t -> with_result
    val xor : t -> t -> with_result
    val xnor : t -> t -> with_result

    module Set : sig
      val mem : t -> int -> bool
      (** [mem v i] checks whenever the bit with offset [i] is set to one. *)

      val inter : t -> t -> with_result
      (** [inter x y] returns the elements that are in both [x] and [y] *)

      val complement : t -> with_result
      (** [complement x] returns bitwise negation of [x] *)

      val symmetric_diff : t -> t -> with_result
      (** [symmetric_diff x y] returns the elements that are in [x] or
          [y] but not both. *)

      val diff : t -> t -> with_result
      (** [diff x y] returns the elements in [x] that are not in [y] *)

      val union : t -> t -> with_result
      (** [union x y] returns the elements that are in [x] or [y] *)

      val are_disjoint : t -> t -> bool
      (** Test if two sets are disjoint.*)

      val is_subset : of_:t -> t -> bool
      (** [subset s1 ~of_] tests whether the bitvector [s1] is a subset of the
          bitvector [of_]. *)

      val cardinality : t -> int
      (** The number of elements in the set *)
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
  val of_bool_list : bool list -> t
  val to_bool_list : t -> bool list
end

module Bit_zero_last : sig
  type nonrec t = t [@@deriving sexp]
  val to_string : t -> string
  val of_string : string -> t
  val of_bool_list : bool list -> t
  val to_bool_list : t -> bool list
end

val length : t -> int
(** Returns length of the given bitvector. *)

val copy : t -> t
(** Creates a copy of bitvector [v]. *)

val append : t -> t -> t
(** Creates a fresh bitvector that is the concatenation of [v1] and [v2]. *)

val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
(** [fold ~init ~f t] folds [f] over [t] from left to right. *)

val map : t -> f:(bool -> bool) -> t
(** [map t ~f] applies [f] to each bit of [t]. *)

val popcount : t -> int
(** Return the count of bits set to one. *)

val set_all : t -> unit
(** Set all bits to one. *)

val clear_all : t -> unit
(** Set all bits to zero. *)

val is_empty : t -> bool
(** Return [true] whenever all bits are zero. *)

val is_full : t -> bool
(** Return [true] whenever all bits are one. *)

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
