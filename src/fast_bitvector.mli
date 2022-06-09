
type t

val max_length : int

val create : length:int -> t


module type Ops := 
  sig
    val set : t -> int -> unit
    val clear : t -> int -> unit

    val equal : t -> t -> bool

    val not : t -> result:t -> t
    val (land) : t -> t -> result:t -> t
    val (lor) : t -> t -> result:t -> t
    val (lxor) : t -> t -> result:t -> t
  end

module Unsafe : Ops

val copy : t -> t
val append : t -> t -> t
val fold : init:'a -> f:('a -> bool -> 'a) -> t -> 'a
val map : t -> f:(bool -> bool) -> t
