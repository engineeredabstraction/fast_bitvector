(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

type t [@@deriving sexp]

val max_length : int

val create : len:int -> t
val create_full : len:int -> t

module type Ops := 
  sig
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
      val intersect : (t -> t -> t) with_result
      val complement : (t -> t) with_result
      val symmetric_difference : (t -> t -> t) with_result
      val difference : (t -> t -> t) with_result
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

val copy : t -> t
val append : t -> t -> t
val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
val map : t -> f:(bool -> bool) -> t

val popcount : t -> int

val set_all : t -> unit
val clear_all : t -> unit

val is_empty : t -> bool
val is_full : t -> bool

