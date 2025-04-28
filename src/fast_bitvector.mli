(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

type t [@@deriving sexp]

val max_length : int

val create : length:int -> t

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
  end

module Unsafe : Ops

include Ops

(* Bit 0 first *)
module Big_endian : sig
  val to_string : t -> string
  val of_string : string -> t
end

(* Bit 0 last *)
module Little_endian : sig
  val to_string : t -> string
  val of_string : string -> t
end

val length : t -> int

val copy : t -> t
val append : t -> t -> t
val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
val map : t -> f:(bool -> bool) -> t

val popcount : t -> int
