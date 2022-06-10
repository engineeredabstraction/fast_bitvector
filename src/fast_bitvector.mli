
type t

val max_length : int

val create : length:int -> t

module type Ops := 
  sig
    type 'a with_result := result:t -> 'a
    val set : t -> int -> unit
    val clear : t -> int -> unit

    val equal : t -> t -> bool

    val not : (t -> t) with_result
    val and_ : (t -> t -> t) with_result
    val or_ : (t -> t -> t) with_result
    val xor : (t -> t -> t) with_result
  end

module Unsafe : Ops

include Ops

val copy : t -> t
val append : t -> t -> t
val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
val map : t -> f:(bool -> bool) -> t
