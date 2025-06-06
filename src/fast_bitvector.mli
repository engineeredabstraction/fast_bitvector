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

module type Ops := sig
  type 'a with_result := result:t -> 'a

  val set : t -> int -> unit
  val clear : t -> int -> unit
  val set_to : t -> int -> bool -> unit
  val get : t -> int -> bool
  val equal : t -> t -> bool
  val not : (t -> t) with_result
  val and_ : (t -> t -> t) with_result
  val or_ : (t -> t -> t) with_result
  val xor : (t -> t -> t) with_result

  module Set : sig
    val mem : t -> int -> bool
    (** [mem v i] checks whenever the bit with offset [i] is set to one. *)

    val intersect : (t -> t -> t) with_result
    (** [intersect ~result x y] returns bitwise and of [x] and [y], on bits
        allocated in [result]. *)

    val complement : (t -> t) with_result
    (** [complement ~result x] returns bitwise negation of [x], on bits
        allocated in [result]. *)

    val symmetric_difference : (t -> t -> t) with_result
    (** [symmetric_difference ~result x y] returns bitwise xor of [x] and [y],
        on bits allocated in [result]. *)

    val difference : (t -> t -> t) with_result
    (** [difference ~result x y] returns bitwise and of [x] and [y], on bits
        allocated in [result]. *)

    val union : (t -> t -> t) with_result
    (** [union ~result x y] returns bitwise or of [x] and [y], on bits allocated
        in [result]. *)
  end

  (* In [With_int], the least significant bit is take to be at the index [bit0_at]. *)
  module With_int : sig
    val or_ : (t -> bit0_at:int -> int -> t) with_result
    (** or with the int at [bit0_at] offset, excess bits in the int are ignored. *)
  end
end

module Unsafe : Ops
include Ops

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
(** Returns length of the given bitvector. *)

val copy : t -> t
(** Creates a copy of bitvector [v]. *)

val append : t -> t -> t
(** Append one bitvector to another. *)

val extend : by:int -> t -> t
(** Append an all-zero bitvector of size [by]. *)

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

val iter : t -> f:(bool -> unit) -> unit
(** Iterate over all bits. *)

val iteri : t -> f:(int -> bool -> unit) -> unit
(** Iterate over all bits and their offsets. *)

val of_iter : ((bool -> unit) -> unit) -> t
(** Convert an iterator into a bitvector. *)

val to_seq : t -> bool Seq.t
(** Return a sequence (pull style) over bits. *)

val of_seq : bool Seq.t -> t
(** Convert a sequence into a bitvector. *)


module Builder : sig
  type t' := t

  type t 

  val create : unit -> t
  val push : t -> bool -> unit
  val to_bitvector : t -> t'
  val reset : t -> unit
end
