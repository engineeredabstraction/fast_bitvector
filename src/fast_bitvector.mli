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

    val not : t -> with_result
    val and_ : t -> t -> with_result
    val or_ : t -> t -> with_result
    val xor : t -> t -> with_result

    module Set : sig
      val mem : t -> int -> bool
      (** [mem v i] checks whenever the bit with offset [i] is set to one. *)

      val intersect : t -> t -> with_result
      (** [intersect x y] returns the elements that are in both [x] and [y] *)

      val complement : t -> with_result
      (** [complement x] returns bitwise negation of [x] *)

      val symmetric_difference : t -> t -> with_result
      (** [symmetric_difference x y] returns the elements that are in [x] or
          [y] but not both. *)

      val difference : t -> t -> with_result
      (** [difference x y] returns the elements in [x] that are not in [y] *)
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

(** {1 Iterators} *)

val fold_left : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
(** [fold ~init ~f b0...bn] is [f (f (f init b0)...) bn], where [b0...bn] are
    individual bits in a bitvector. *)

val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
(** Alias for [fold_left] *)

val fold_lefti : t -> init:'a -> f:('a -> int -> bool -> 'a) -> 'a
(** [fold_lefti] is [fold_left] with offset provided. *)

val fold_left_set : t -> init:'a -> f:('a -> int -> 'a) -> 'a
(** fold over all offsets of set bits. *)

val fold_set : t -> init:'a -> f:('a -> int -> 'a) -> 'a
(** Alias for [fold_left_set] *)

val foldi : t -> init:'a -> f:('a -> int -> bool -> 'a) -> 'a
(** Alias for [fold_lefti] *)

val fold_right : t -> init:'a -> f:(bool -> 'a -> 'a) -> 'a
(** [fold ~init ~f b0...bn] is [f (f (f init b0)...) bn], where [b0...bn] are
    individual bits in a bitvector. *)

val fold_righti : t -> init:'a -> f:(int -> bool -> 'a -> 'a) -> 'a
(** [foldi] is [fold] with offset provided. *)

val fold_right_set : t -> init:'a -> f:(int -> 'a -> 'a) -> 'a
(** fold over all offsets of set bits. *)

val map : t -> f:(bool -> bool) -> t
(** Map every bit in the vector with function [f]. *)

val mapi : t -> f:(int -> bool -> bool) -> t
(** [mapi ~f b0...bn] is [f 0 b0 ... f n bn], where [bi] is [i]-th bit in a
    bitvector.*)

val iter : t -> f:(bool -> unit) -> unit
(** Iterate over all bits. *)

val iteri : t -> f:(int -> bool -> unit) -> unit
(** Iterate over all bits and their offsets. *)

val iter_set : t -> f:(int -> unit) -> unit
(** Iterate over all offsets of set bits. *)

val rev_iteri : t -> f:(int -> bool -> unit) -> unit
(** Iterate over all bits and their offsets in reverse order. *)

val rev_iter : t -> f:(bool -> unit) -> unit
(** Iterate over all bits in reverse order. *)

val rev_iter_set : t -> f:(int -> unit) -> unit
(** Iterate over all offsets of set bits in reverse order. *)

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

module Private : sig
  module Bitops = Bitops
end
