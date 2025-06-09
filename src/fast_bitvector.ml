(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

let (=) = Int.equal

type t = Bytes.t

let failwithf s = Printf.ksprintf failwith s

module type Element = sig
  type t

  val bit_size : int
  val byte_size : int
  val shift : int

  val equal : t -> t -> bool

  val to_int : t -> int
  val of_int : int -> t

  val get : bytes -> int -> t
  val set : bytes -> int -> t -> unit

  val zero : t
  val one : t
  val minus_one : t

  val sub : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right_logical : t -> int -> t

  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val count_set_bits : t -> int
end

module Element_32 = struct
  include Int32

  let bit_size = 32
  let byte_size = 4
  let shift = 5

  external get : bytes -> int -> t = "%caml_bytes_get32u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set32u"

  let set b i v = set b (i*byte_size) v
  let get b i = get b (i*byte_size)

  let count_set_bits = Popcount.count_set_bits_32

  let of_int i =
    logand (of_int i) max_int
end

module Element_64 = struct
  include Int64

  let bit_size = 64
  let byte_size = 8
  let shift = 6

  external get : bytes -> int -> t = "%caml_bytes_get64u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set64u"

  let set b i v = set b (i*byte_size) v
  let get b i = get b (i*byte_size)

  let count_set_bits = Popcount.count_set_bits_64

  let of_int i =
    logand (of_int i) max_int
end

module Element = (val
                   if Sys.word_size = 32
                    then (module Element_32 : Element)
                    else (module Element_64 : Element)
                 )

let length t =
  Element.get t 0
  |> Element.to_int

let max_length =
  ((Sys.max_string_length / Element.byte_size) - 1) * Element.bit_size

let [@inline always] total_words ~length =
  (length + Element.bit_size - 1) lsr Element.shift

let create ~len:new_length =
  if new_length > max_length
  then failwithf "length %d exceeds maximum length %d" new_length max_length;
  let total_data_words = (new_length + Element.bit_size - 1) / Element.bit_size in
  let total_words = total_data_words + 1 in
  let t = Bytes.init (total_words * Element.byte_size) (fun _ -> '\x00') in
  Element.set t 0 (Element.of_int new_length);
  assert (length t == new_length);
  t

let [@inline always] loop_set result value =
  let length = length result in
  let total_words = total_words ~length in
  for i = 1 to total_words do
    Element.set result i
      value
  done

let set_all t =
  loop_set t Element.minus_one

let clear_all t =
  loop_set t Element.zero

external (&&&) : bool -> bool -> bool = "%andint"
external (|||) : bool -> bool -> bool = "%orint"

let [@inline always] foldop1 ~init ~f ~final a =
  let length = length a in
  let total_words = total_words ~length in
  let acc = ref init in
  for i = 1 to pred total_words do
    acc :=
      (f [@inlined hint])
        !acc
        (Element.get a i)
  done;
  let remaining = length mod Element.bit_size in
  if remaining = 0
  then begin
    (f [@inlined hint])
      !acc
      (Element.get a total_words)
  end else begin
    let mask = Element.(sub (shift_left one remaining) one) in
    (f [@inlined hint])
      !acc
      (final ~mask (Element.get a total_words))
  end

let popcount t =
  foldop1 t 
    ~init:0
    ~f:(fun acc v -> acc + (Element.count_set_bits v))
    ~final:(fun ~mask a -> Element.logand mask a)

let is_empty t =
  foldop1 t
    ~init:true
    ~f:(fun acc v -> acc &&& (Element.equal v Element.zero))
    ~final:(fun ~mask a -> Element.logand mask a)

let is_full t =
  foldop1 t
    ~init:true
    ~f:(fun acc v -> acc &&& (Element.equal v Element.minus_one))
    ~final:(fun ~mask a -> Element.logor (Element.lognot mask) a)

module type Check = sig
  val index : t -> int -> unit

  val length2 : t -> t -> int
  val length3 : t -> t -> t -> int
end

module type Make_result = sig
  type t' := t

  type _ t

  val wrap_1 : (t' -> t' -> unit) -> (t' -> _ t)
  val wrap_2 : (t' -> t' -> t' -> unit) -> (t' -> t' -> _ t)
end

module Explicit_result = struct
  type t' = t

  type _ t = dst: t' -> unit

  let [@inline always] wrap_1 (f : t' -> t' -> unit) : (t' -> 'a t) =
    fun bv ~dst ->
      (f [@inlined hint]) bv dst

  let [@inline always] wrap_2 (f : t' -> t' -> t' -> unit) : (t' -> t' -> 'a t) =
    fun bv1 bv2 ~dst ->
      (f [@inlined hint]) bv1 bv2 dst
end

module _ : Make_result =
  Explicit_result

module Allocate_result = struct
  type t' = t

  type _ t = t'

  let [@inline always] wrap_1 (f : t' -> t' -> unit) : (t' -> 'a t) =
    fun bv ->
      let res_bv = create ~len:(length bv) in
      f bv res_bv;
      res_bv

  let [@inline always] wrap_2 (f : t' -> t' -> t' -> unit) : (t' -> t' -> 'a t) =
    fun bv1 bv2 ->
      let res_bv = create ~len:(length bv1) in
      f bv1 bv2 res_bv;
      res_bv

end

module [@inline always] Ops(Check : Check)(Make_result : Make_result) = struct
  let [@inline always] logop1 ~f =
    let [@inline always] inner_f a result =
      let length = Check.length2 a result in
      let total_words = total_words ~length in
      for i = 1 to total_words do
        Element.set result i
          (f
             (Element.get a i)
          )
      done;
    in
    Make_result.wrap_1 inner_f


  let [@inline always] logop2 ~f =
    let [@inline always] inner_f a b result =
      let length = Check.length3 a b result in
      let total_words = total_words ~length in
      for i = 1 to total_words do
        Element.set result i
          (f
             (Element.get a i)
             (Element.get b i)
          )
      done;
    in
    Make_result.wrap_2 inner_f

  let [@inline always] foldop2 ~init ~f ~final a b =
    let length = Check.length2 a b in
    let total_words = total_words ~length in
    let acc = ref init in
    for i = 1 to pred total_words do
      acc :=
        (f [@inlined hint])
          !acc
          (Element.get a i)
          (Element.get b i)
    done;
    let remaining = length land (Element.bit_size - 1) in
    let mask = Element.sub (Element.shift_left Element.one remaining) Element.one in
    (f [@inlined hint])
      !acc
      (final ~mask (Element.get a total_words))
      (final ~mask (Element.get b total_words))

  let [@inline always] get t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    Element.logand
      (Element.shift_right_logical v subindex)
      Element.one
    |> Element.to_int
    |> (Obj.magic : int -> bool)

  let [@inline always] set t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let v' =
      Element.logor v (Element.shift_left Element.one subindex)
    in
    Element.set t index v'

  let [@inline always] set_to t i b =
    Check.index t i;
    let b = Element.of_int ((Obj.magic : bool -> int) b) in
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let mask = Element.lognot (Element.shift_left Element.one subindex) in
    let v' =
      Element.logor
        (Element.logand v mask)
        (Element.shift_left b subindex)
    in
    Element.set t index v'

  let [@inline always] clear t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let v' =
      Element.logand v (Element.lognot (Element.shift_left Element.one subindex))
    in
    Element.set t index v'

  let equal a b =
    (length a = length b)
    &&
    foldop2 a b
      ~init:true
      ~f:(fun acc a b ->
          acc
          &&&
          (Element.equal Element.zero (Element.logxor a b))
        )
      ~final:(fun ~mask a -> Element.logand mask a)

  let [@inline always] not a = logop1 ~f:Element.lognot a

  let [@inline always] and_ a b = logop2 ~f:Element.logand a b

  let [@inline always] or_ a b = logop2 ~f:Element.logor a b

  let [@inline always] xor a b = logop2 ~f:Element.logxor a b

  module Set = struct
    let mem = get

    let intersect = and_
    let complement = not
    let symmetric_difference = xor

    let [@inline always] difference a b =
      logop2 ~f:(fun a b ->
          Element.logand a (Element.lognot b)
        ) a b
  end

end

module Check_none = struct
  let [@inline always] index _ _ = ()
  let [@inline always] length2 a _ = length a
  let [@inline always] length3 a _ _ = length a
end

module Check_all = struct
  let [@cold] raise_index_out_of_bounds ~i ~length =
    failwithf "index %d out of bounds [0,%d)" i length

  let [@inline always] index t i =
    let length = length t in
    if (i >= length) ||| (i < 0) then raise_index_out_of_bounds ~i ~length

  let [@cold] raise_length_mismatch2 ~length1 ~length2 =
    failwithf "length mismatch: %d <> %d" length1 length2

  let [@inline always] length2 a b =
    let la = length a in
    let lb = length b in
    if la <> lb then raise_length_mismatch2 ~length1:la ~length2:lb;
    la

  let [@cold] raise_length_mismatch3 ~length1 ~length2 ~length3 =
    failwithf "length mismatch: %d, %d, %d must be equal" length1 length2 length3

  let [@inline always] length3 a b c =
    let la = length a in
    let lb = length b in
    let lc = length c in
    if (la <> lb) ||| (la <> lc)
    then raise_length_mismatch3 ~length1:la ~length2:lb ~length3:lc;
    la
end

module Unsafe = Ops(Check_none)(Explicit_result)

include Ops(Check_all)(Explicit_result)

module Allocate = struct
  module Unsafe = Ops(Check_none)(Allocate_result)

  include Ops(Check_all)(Allocate_result)
end

let equal a b =
  let la = length a in
  let lb = length b in
  la = lb && Unsafe.equal a b

let init new_length ~f =
  let t = create ~len:new_length in
  for i = 0 to new_length - 1 do
    Unsafe.set_to t i ((f [@inlined hint]) i);
  done;
  t

let create_full ~len =
  let t = create ~len in
  Unsafe.not ~dst:t t;
  t

let copy t =
  Bytes.copy t

let append a b =
  let length_a = length a in
  let length_b = length b in
  let length = length_a + length_b in
  let words_a = total_words ~length:length_a in
  let t = create ~len:length in
  for i = 1 to words_a do
    Element.set t i (Element.get a i)
  done;
  for i = 0 to pred length_b do
    let target = length_a + i in
    let source_value = Unsafe.get b i in
    Unsafe.set_to t target source_value
  done;
  t

let [@inline always] fold ~init ~f t =
  let length = length t in
  let acc = ref init in
  for i = 0 to pred length do
    (* CR smuenzel: process word at a time *)
    acc := f !acc (Unsafe.get t i)
  done;
  !acc

let map t ~f =
  (init [@inlined hint]) (length t)
    ~f:(fun i -> f (Unsafe.get t i))

open Sexplib0

module type Bit_ordering_spec = sig
  val sexp_name : string
  val get_index : length:int -> i:int -> int
end

module Bit_ordering_conversion(Spec : Bit_ordering_spec) = struct
  type nonrec t = t

  let to_string t =
    let length = length t in
    (String.init [@inlined hint]) length (fun i ->
        if Unsafe.get t (Spec.get_index ~length ~i)
        then '1'
        else '0'
      )

  let of_string s =
    let length = String.length s in
    init length
      ~f:(fun i ->
          match String.unsafe_get s (Spec.get_index ~length ~i) with
          | '0' -> false
          | '1' -> true
          | other ->
            failwithf "invalid char '%c'" other
        )

  let sexp_of_t t =
    Sexp.List
      [ Sexp.Atom Spec.sexp_name
      ; Sexp.Atom (to_string t)
      ]
end

module Bit_zero_first' =
  Bit_ordering_conversion(struct
      let sexp_name = "B0F"
      let get_index ~length:_ ~i = i
    end)

module Bit_zero_last' =
  Bit_ordering_conversion(struct
      let sexp_name = "B0L"
      let get_index ~length ~i = length - (i + 1)
    end)

let t_of_sexp = function
  | Sexp.List
      [ Sexp.Atom ("BE" | "B0F")
      ; Sexp.Atom s
      ] -> Bit_zero_first'.of_string s
  | Sexp.List
      [ Sexp.Atom ("LE" | "B0L")
      ; Sexp.Atom s
      ]
  | Sexp.Atom s ->
    Bit_zero_last'.of_string s
  | other ->
    Sexp_conv.of_sexp_error "not a bitvector" other

let sexp_of_t = Bit_zero_last'.sexp_of_t

module Bit_zero_first = struct
  include Bit_zero_first'

  let t_of_sexp = t_of_sexp
end

module Bit_zero_last = struct
  include Bit_zero_last'

  let t_of_sexp = t_of_sexp
end
