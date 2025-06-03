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

type 'a with_result := result:t -> 'a

module type Set := sig
  val mem : t -> int -> bool
  (** [mem v i] checks whenever the bit with offset [i] is set to one. *)

  val intersect : (t -> t -> t) with_result
  (** [intersect ~result x y] returns bitwise and of [x] and [y], on bits
      allocated in [result]. *)

  val complement : (t -> t) with_result
  (** [complement ~result x] returns bitwise negation of [x], on bits allocated
      in [result]. *)

  val symmetric_difference : (t -> t -> t) with_result
  (** [symmetric_difference ~result x y] returns bitwise xor of [x] and [y], on
      bits allocated in [result]. *)

  val difference : (t -> t -> t) with_result
  (** [difference ~result x y] returns bitwise and of [x] and [y], on bits
      allocated in [result]. *)

  val union : (t -> t -> t) with_result
  (** [union ~result x y] returns bitwise or of [x] and [y], on bits allocated
      in [result]. *)
end

module type Ops := sig
  val set : t -> int -> unit
  val clear : t -> int -> unit
  val set_to : t -> int -> bool -> unit
  val get : t -> int -> bool
  val equal : t -> t -> bool
  val not : (t -> t) with_result
  val and_ : (t -> t -> t) with_result
  val or_ : (t -> t -> t) with_result
  val xor : (t -> t -> t) with_result

  module Set : Set
end

module Unsafe : Ops

(** Relaxed set operations: iteration is done on result vector, missing bits in
    operands are considered zero automatically. *)
module Relaxed : sig
  include Set

  val equal : t -> t -> bool
end

include Ops

(* Bit 0 first *)
module Big_endian : sig
  type nonrec t = t [@@deriving sexp]

  val to_string : t -> string
  val to_debug_string : t -> string
  val of_string : string -> t
end

(* Bit 0 last *)
module Little_endian : sig
  type nonrec t = t [@@deriving sexp]

  val to_string : t -> string
  val to_debug_string : t -> string
  val of_string : string -> t
end

val length : t -> int
(** Returns length of the given bitvector. *)

val copy : t -> t
(** Creates a copy of bitvector [v]. *)

val append : t -> t -> t
(** Append one bitvector to another. *)

val extend : t -> len:int -> t
(** Append a bitvector to [len], new bits are zeroed. *)

val extend_inplace : t -> len:int -> t
(** Resize bitvector to accomodate [len] bits, or allocate bigger bitvector like
    [extend]. New bits may have random values. Invalidates input vector. *)

val popcount : t -> int
(** Return the count of bits set to one. *)

val set_all : t -> unit
(** Set all bits one. *)

val clear_all : t -> unit
(** Set all bits zero. *)

val is_empty : t -> bool
(** Return whenever all bits are zero. *)

val is_full : t -> bool
(** Return whenever all bits are one. *)

(** {1 Iterators} *)

val fold : t -> init:'a -> f:('a -> bool -> 'a) -> 'a
(** [fold ~init ~f b0...bn] is [f (f (f init b0)...) bn], where [b0...bn] are
    individual bits in a bitvector. *)

val foldi : t -> init:'a -> f:('a -> int -> bool -> 'a) -> 'a
(** [foldi] is [fold] with offset provided. *)

val map : t -> f:(bool -> bool) -> t
(** Map every bit in the vector with function [f]. *)

val mapi : t -> f:(int -> bool -> bool) -> t
(** [mapi ~f b0...bn] is [f 0 b0 ... f n bn], where [bi] is [i]-th bit in a
    bitvector.*)

val iter : t -> f:(bool -> unit) -> unit
(** Iterate over all bits. *)

val iteri : t -> f:(int -> bool -> unit) -> unit
(** Iterate over all bits and their offsets. *)

val iter_seti : t -> f:(int -> unit) -> unit
(** Iterate over all offsets of set bits. *)

(** {1 Conversions} *)

val of_bool_iter : ((bool -> unit) -> unit) -> t
(** Convert a bool iterator into a bitvector. *)

val to_bool_seq : t -> bool Seq.t
(** Return a bool sequence (pull style) over bits. *)

val of_bool_seq : bool Seq.t -> t
(** Convert a bool sequence into a bitvector. *)

val of_offset_iter : ((int -> unit) -> unit) -> t
(** Convert an offset iterator into a bitvector. *)

val to_offset_seq : t -> int Seq.t
(** Return an offset sequence (pull style) over bits. *)

val of_offset_seq : int Seq.t -> t
(** Convert an offset sequence into a bitvector. *)
