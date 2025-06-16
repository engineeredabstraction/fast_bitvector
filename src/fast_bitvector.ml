(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

let ( = ) = Int.equal
let ( <> ) x y = not @@ Int.equal x y

type t = Bytes.t

let failwithf s = Printf.ksprintf failwith s

module type Element = sig
  type t

  val bit_size : int
  val byte_size : int
  val shift : int
  val index_mask : int
  val last_bit : t
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
  val sexp_of_t : t -> Sexplib0.Sexp.t
end

module Element_32 = struct
  include Int32

  let bit_size = 32
  let byte_size = 4
  let shift = 5
  let index_mask = 31
  let last_bit = shift_left one index_mask

  external get : bytes -> int -> t = "%caml_bytes_get32u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set32u"

  let[@inline always] set b i v = set b (i * byte_size) v
  let[@inline always] get b i = get b (i * byte_size)
  let count_set_bits = Popcount.count_set_bits_32
  let sexp_of_t x = Sexplib0.Sexp_conv.sexp_of_int32 x
  let of_int i = logand (of_int i) max_int
end

module Element_64 = struct
  include Int64

  let bit_size = 64
  let byte_size = 8
  let shift = 6
  let index_mask = 63
  let last_bit = shift_left one index_mask

  external get : bytes -> int -> t = "%caml_bytes_get64u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set64u"

  let[@inline always] set b i v = set b (i * byte_size) v
  let[@inline always] get b i = get b (i * byte_size)
  let count_set_bits = Popcount.count_set_bits_64
  let sexp_of_t x = Sexplib0.Sexp_conv.sexp_of_int64 x
  let of_int i = logand (of_int i) max_int
end

module type Foldable = sig
  type t

  val fold_lefti : t -> init:'acc -> f:('acc -> int -> bool -> 'acc) -> 'acc
  val fold_righti : t -> init:'acc -> f:(int -> bool -> 'acc -> 'acc) -> 'acc
end

module type IterationS = sig
  include Foldable

  val fold_left : t -> init:'acc -> f:('acc -> bool -> 'acc) -> 'acc
  val fold_right : t -> init:'acc -> f:(bool -> 'acc -> 'acc) -> 'acc
  val iteri : t -> f:(int -> bool -> unit) -> unit
  val rev_iteri : t -> f:(int -> bool -> unit) -> unit
  val iter : t -> f:(bool -> unit) -> unit
  val rev_iter : t -> f:(bool -> unit) -> unit
  val iter_seti : t -> f:(int -> unit) -> unit
  val rev_iter_seti : t -> f:(int -> unit) -> unit
end

module [@inline always] Iteration (F : Foldable) :
  IterationS with type t := F.t = struct
  include F

  let fold_left t ~init ~f = fold_lefti ~init ~f:(fun acc _i v -> f acc v) t
  let fold_right t ~init ~f = fold_righti ~init ~f:(fun _i v acc -> f v acc) t
  let iteri t ~f = fold_lefti ~init:() ~f:(fun _acc i b -> f i b) t
  let iter t ~f = iteri ~f:(fun _i b -> f b) t
  let rev_iteri t ~f = fold_righti ~init:() ~f:(fun i b _acc -> f i b) t
  let rev_iter t ~f = rev_iteri ~f:(fun _i b -> f b) t
  let iter_seti v ~f = iteri ~f:(fun i b -> if b then f i) v
  let rev_iter_seti v ~f = rev_iteri ~f:(fun i b -> if b then f i) v
end

module Element = struct
  include
    (val if Sys.word_size = 32 then (module Element_32 : Element)
         else (module Element_64 : Element))

  let to_debug_string t =
    (String.init [@inlined hint]) bit_size (fun i ->
        if equal (logand (shift_right_logical t i) one) one then '1' else '0')

  include Iteration (struct
    type nonrec t = t

    let[@inline always] fold_lefti e ~init ~f =
      let acc = ref init in
      let e = ref e in
      for i = 0 to pred bit_size do
        let raw_bit = logand one !e in
        let bit = raw_bit |> to_int |> (Obj.magic : int -> bool) in
        e := shift_right_logical !e 1;
        acc := f !acc i bit
      done;
      !acc

    let[@inline always] fold_righti e ~init ~f =
      let acc = ref init in
      let e = ref e in
      for i = bit_size - 1 downto 0 do
        let raw_bit = logand last_bit !e in
        let bit = equal raw_bit last_bit in
        e := shift_left !e 1;
        acc := f i bit !acc
      done;
      !acc
  end)

  let of_iteri iter =
    let result = ref zero in
    iter (fun (i, bit) ->
        let bit = bit |> (Obj.magic : bool -> int) |> of_int in
        let bit = shift_left bit i in
        result := logor bit !result);
    !result

  let of_iter iter = of_iteri (Iter.zip_i iter)

  let%test_unit _ =
    let seven = of_int 7 in
    let restored = of_iter (fun f -> iter ~f seven) in
    [%test_eq: t] seven restored

  let[@inline always] get_or_zero e i word_size =
    if i <= word_size then get e i else zero
end

let length t = Element.get t 0 |> Element.to_int

let max_length =
  ((Sys.max_string_length / Element.byte_size) - 1) * Element.bit_size

let[@inline always] total_words ~length =
  (length + Element.bit_size - 1) lsr Element.shift

let[@inline always] capacity t = (8 * Bytes.length t) - Element.bit_size
let[@inline always] total_full_words ~length = length lsr Element.shift

let create ~len:new_length =
  if new_length > max_length then
    failwithf "length %d exceeds maximum length %d" new_length max_length;
  let total_data_words =
    (new_length + Element.bit_size - 1) / Element.bit_size
  in
  let total_words = total_data_words + 1 in
  let t = Bytes.make (total_words * Element.byte_size) '\x00' in
  Element.set t 0 (Element.of_int new_length);
  assert (length t == new_length);
  t

let set_all result =
  let length = length result in
  let total_full_words = total_full_words ~length in
  let remainder_length = length - (Element.bit_size * total_full_words) in
  for i = 1 to total_full_words do
    Element.set result i Element.minus_one
  done;
  if remainder_length >= 0 then
    Element.set result (total_full_words + 1)
      (Element.of_iter (Iter.repeat true |> Iter.take remainder_length))

let clear_all result =
  let length = length result in
  let total_words = total_words ~length in
  for i = 1 to total_words do
    Element.set result i Element.zero
  done

external ( &&& ) : bool -> bool -> bool = "%andint"
external ( ||| ) : bool -> bool -> bool = "%orint"

let[@inline always] foldi ~init ~f v =
  let length = length v in
  let total_words = total_words ~length in
  let acc = ref init in
  for i = 1 to total_words do
    acc := (f [@inlined hint]) !acc i (Element.get v i)
  done;
  !acc

let popcount t =
  foldi t ~init:0 ~f:(fun acc _ v -> acc + Element.count_set_bits v)

let is_empty t =
  foldi t ~init:true ~f:(fun acc _ v -> acc &&& Element.equal v Element.zero)

let is_full v =
  let length = length v in
  if length = 0 then false
  else
    let total_words = total_words ~length in
    let remaining = length land (Element.bit_size - 1) in
    (*remainder of division by bitsize*)
    let mask =
      if remaining = 0 then Element.minus_one
      else Element.sub (Element.shift_left Element.one remaining) Element.one
    in
    let inverse_mask = Element.lognot mask in
    foldi v ~init:true ~f:(fun acc i e ->
        let e =
          if i <> 0 && i = total_words then Element.logor inverse_mask e else e
        in
        acc &&& Element.equal e Element.minus_one)

module type Check = sig
  val index : t -> int -> unit
  val length2 : t -> t -> int
  val length3 : t -> t -> t -> int
end

let[@inline always] logop1_relaxed ~f a result =
  let length = length result
  and total_words_a = total_words ~length:(length a) in
  let total_words = total_words ~length in
  for i = 1 to total_words do
    Element.set result i (f (Element.get_or_zero a i total_words_a))
  done;
  result

let[@inline always] logop2_relaxed ~f a b result =
  let total_words = total_words ~length:(length result)
  and total_words_a = total_words ~length:(length a)
  and total_words_b = total_words ~length:(length b) in
  for i = 1 to total_words do
    Element.set result i
      (f
         (Element.get_or_zero a i total_words_a)
         (Element.get_or_zero b i total_words_b))
  done;
  result

module [@inline always] Ops (Check : Check) = struct
  let[@inline always] logop1 ~f a result =
    let length = Check.length2 a result in
    let total_words = total_words ~length in
    for i = 1 to total_words do
      Element.set result i (f (Element.get a i))
    done;
    result

  let[@inline always] logop2 ~f a b result =
    let length = Check.length3 a b result in
    let total_words = total_words ~length in
    for i = 1 to total_words do
      Element.set result i (f (Element.get a i) (Element.get b i))
    done;
    result

  let[@inline always] get t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    Element.logand (Element.shift_right_logical v subindex) Element.one
    |> Element.to_int
    |> (Obj.magic : int -> bool)

  let[@inline always] set t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let v' = Element.logor v (Element.shift_left Element.one subindex) in
    Element.set t index v'

  let[@inline always] set_to t i b =
    Check.index t i;
    let b = Element.of_int ((Obj.magic : bool -> int) b) in
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let mask = Element.lognot (Element.shift_left Element.one subindex) in
    let v' =
      Element.logor (Element.logand v mask) (Element.shift_left b subindex)
    in
    Element.set t index v'

  let[@inline always] clear t i =
    Check.index t i;
    let index = 1 + (i lsr Element.shift) in
    let subindex = i land (Element.bit_size - 1) in
    let v = Element.get t index in
    let v' =
      Element.logand v
        (Element.lognot (Element.shift_left Element.one subindex))
    in
    Element.set t index v'

  let not ~result a = logop1 ~f:Element.lognot a result
  let and_ ~result a b = logop2 ~f:Element.logand a b result
  let or_ ~result a b = logop2 ~f:Element.logor a b result
  let xor ~result a b = logop2 ~f:Element.logxor a b result
  let mem = get
  let inter = and_
  let complement = not
  let symmetric_diff = xor

  let diff ~result a b =
    logop2 ~f:(fun a b -> Element.logand a (Element.lognot b)) a b result

  let union = or_

  let equal a b =
    let _ = Check.length2 a b in
    foldi a ~init:true ~f:(fun acc i a ->
        acc &&& Element.equal Element.zero (Element.logxor a (Element.get b i)))

  let subset a b =
    let _ = Check.length2 a b in
    foldi ~init:true
      ~f:(fun acc i e_b ->
        acc
        &&&
        let e_a = Element.get a i in
        Element.equal (Element.logand e_a e_b) e_a)
      b

  let disjoint a b =
    let _ = Check.length2 a b in
    foldi ~init:true
      ~f:(fun acc i e_b ->
        acc
        &&&
        let e_a = Element.get a i in
        Element.equal Element.zero (Element.logand e_a e_b))
      b

  let equal_modulo ~modulo a b =
    let _ = Check.length3 a b modulo in
    foldi ~init:true
      ~f:(fun acc i e_m ->
        acc
        &&&
        let e_a = Element.get a i and e_b = Element.get b i in
        Element.equal (Element.logand e_a e_m) (Element.logand e_b e_m))
      modulo

  module With_int = struct
    let or_ ~result a ~bit0_at b_int =
      Check.index a bit0_at;
      let b_el = Element.of_int b_int in
      let bit0_element_index = 1 + (bit0_at lsr Element.shift) in
      let bitN_element_index =
        1 + ((bit0_at + Sys.int_size - 1) lsr Element.shift)
      in
      let should_set_N =
        (bitN_element_index - 1) * Element.bit_size < length a
      in
      let element0_shift_left = bit0_at land Element.index_mask in
      let elementN_shift_right =
        (* Only those bits that exceed Element.bit_size after shifting element0_shift_left
           need to be set in elementN.
           So we examine the highest bit in b_el (a position Sys.int_size - 1):
        *)
        let excess_bits = Element.bit_size - Sys.int_size in
        let bits_in_elementN = element0_shift_left - excess_bits in
        Sys.int_size - bits_in_elementN
      in
      let v = Element.get a bit0_element_index in
      let v' = Element.logor v (Element.shift_left b_el element0_shift_left) in
      Element.set result bit0_element_index v';
      (if bit0_element_index <> bitN_element_index && should_set_N then
         let v = Element.get a bitN_element_index in
         let v' =
           Element.logor v
             (Element.shift_right_logical b_el elementN_shift_right)
         in
         Element.set result bitN_element_index v');
      result
  end
end

module Check_none = struct
  let[@inline always] index _ _ = ()
  let[@inline always] length2 _ r = length r
  let[@inline always] length3 _ _ r = length r
end

module Check_all = struct
  let[@cold] raise_index_out_of_bounds ~i ~length =
    failwithf "index %d out of bounds [0,%d)" i length

  let[@inline always] index t i =
    let length = length t in
    if i >= length ||| (i < 0) then raise_index_out_of_bounds ~i ~length

  let[@cold] raise_length_mismatch2 ~length1 ~length2 =
    failwithf "length mismatch: %d <> %d" length1 length2

  let[@inline always] length2 a b =
    let la = length a in
    let lb = length b in
    if la <> lb then raise_length_mismatch2 ~length1:la ~length2:lb;
    la

  let[@cold] raise_length_mismatch3 ~length1 ~length2 ~length3 =
    failwithf "length mismatch: %d, %d, %d must be equal" length1 length2
      length3

  let[@inline always] length3 a b c =
    let la = length a in
    let lb = length b in
    let lc = length c in
    if la <> lb ||| (la <> lc) then
      raise_length_mismatch3 ~length1:la ~length2:lb ~length3:lc;
    la
end

module Unsafe = Ops (Check_none)
include Ops (Check_all)

module Relaxed = struct
  let mem v i = if i < length v then get v i else false
  let inter ~result a b = logop2_relaxed ~f:Element.logand a b result
  let union ~result a b = logop2_relaxed ~f:Element.logor a b result
  let complement ~result a = logop1_relaxed ~f:Element.lognot a result
  let symmetric_diff ~result a b = logop2_relaxed ~f:Element.logxor a b result

  let diff ~result a b =
    logop2_relaxed
      ~f:(fun a b -> Element.logand a (Element.lognot b))
      a b result

  let equal a b =
    let a, b = if length a >= length b then (a, b) else (b, a) in
    let total_words_b = total_words ~length:(length b) in
    foldi a ~init:true ~f:(fun acc i a ->
        acc
        &&& Element.equal Element.zero
              (Element.logxor a (Element.get_or_zero b i total_words_b)))

  let subset a b =
    let total_words_a = total_words ~length:(length a)
    and total_words_b = total_words ~length:(length b) in
    let total_words = Int.max total_words_a total_words_b in
    Iter.(1 -- total_words)
    |> Iter.fold
         (fun acc i ->
           acc
           &&&
           let e_a = Element.get_or_zero a i total_words_a
           and e_b = Element.get_or_zero b i total_words_b in
           Element.equal (Element.logand e_a e_b) e_a)
         true

  let disjoint a b =
    let total_words_a = total_words ~length:(length a)
    and total_words_b = total_words ~length:(length b) in
    let total_words = Int.max total_words_a total_words_b in
    Iter.(1 -- total_words)
    |> Iter.fold
         (fun acc i ->
           acc
           &&&
           let e_a = Element.get_or_zero a i total_words_a
           and e_b = Element.get_or_zero b i total_words_b in
           Element.equal Element.zero (Element.logand e_a e_b))
         true

  let equal_modulo ~modulo a b =
    let total_words_a = total_words ~length:(length a)
    and total_words_b = total_words ~length:(length b) in
    foldi ~init:true
      ~f:(fun acc i e_m ->
        acc
        &&&
        let e_a = Element.get_or_zero a i total_words_a
        and e_b = Element.get_or_zero b i total_words_b in
        Element.equal (Element.logand e_a e_m) (Element.logand e_b e_m))
      modulo
end

let init new_length ~f =
  let t = create ~len:new_length in
  for i = 0 to new_length - 1 do
    Unsafe.set_to t i ((f [@inlined hint]) i)
  done;
  t

let create_full ~len =
  let t = create ~len in
  set_all t;
  t

let copy t = Bytes.copy t

let append a b =
  let length_a = length a in
  let length_b = length b in
  let length = length_a + length_b in
  let t = create ~len:length in
  Bytes.blit a Element.byte_size t Element.byte_size ((length_a + 7) / 8);
  for i = 0 to pred length_b do
    Unsafe.set_to t (length_a + i) (Unsafe.get b i)
  done;
  t

let extend v ~len =
  assert (len > length v);
  let new_vec = create ~len in
  let byte_size = (length v + 7) / 8 in
  Bytes.blit v Element.byte_size new_vec Element.byte_size byte_size;
  new_vec

let extend_inplace v ~len =
  let prev_length = length v in
  if prev_length < len then
    let prev_capacity = capacity v in
    let new_vec =
      if len <= prev_capacity then (
        Element.set v 0 (Element.of_int len);
        v)
      else extend v ~len
    in
    new_vec
  else v

include Iteration (struct
  type nonrec t = t

  let[@inline always] fold_lefti v ~init ~f =
    let length = length v in
    let acc = ref init in
    for wi = 1 to total_words ~length do
      let bucket = (wi - 1) lsl Element.shift in
      let w = Element.get v wi in
      acc :=
        Element.fold_lefti ~init:!acc
          ~f:(fun acc i b ->
            if bucket + i < length then f acc (bucket + i) b else acc)
          w
    done;
    !acc

  let[@inline always] fold_righti v ~init ~f =
    let length = length v in
    let acc = ref init in
    for wi = total_words ~length downto 1 do
      let bucket = (wi - 1) lsl Element.shift in
      let w = Element.get v wi in
      acc :=
        Element.fold_righti ~init:!acc
          ~f:(fun i b acc ->
            if bucket + i < length then f (bucket + i) b acc else acc)
          w
    done;
    !acc
end)

let map t ~f =
  (init [@inlined hint]) (length t) ~f:(fun i -> f (Unsafe.get t i))

let mapi t ~f =
  (init [@inlined hint]) (length t) ~f:(fun i -> f i (Unsafe.get t i))

open Sexplib0

module type Bit_ordering_spec = sig
  val sexp_name : string
  val get_index : length:int -> i:int -> int
end

module Bit_ordering_conversion (Spec : Bit_ordering_spec) = struct
  type nonrec t = t

  let to_string t =
    let length = length t in
    (String.init [@inlined hint]) length (fun i ->
        if Unsafe.get t (Spec.get_index ~length ~i) then '1' else '0')

  let to_debug_string t =
    let length = capacity t in
    (String.init [@inlined hint]) length (fun i ->
        if Unsafe.get t (Spec.get_index ~length ~i) then '1' else '0')

  let of_string s =
    let length = String.length s in
    init length ~f:(fun i ->
        match String.unsafe_get s (Spec.get_index ~length ~i) with
        | '0' -> false
        | '1' -> true
        | other -> failwithf "invalid char '%c'" other)

  let sexp_of_t t =
    Sexp.List [ Sexp.Atom Spec.sexp_name; Sexp.Atom (to_string t) ]
end

module Bit_zero_first' = Bit_ordering_conversion (struct
  let sexp_name = "B0F"
  let get_index ~length:_ ~i = i
end)

module Bit_zero_last' = Bit_ordering_conversion (struct
  let sexp_name = "B0L"
  let get_index ~length ~i = length - (i + 1)
end)

let t_of_sexp = function
  | Sexp.List [ Sexp.Atom ("BE" | "B0F"); Sexp.Atom s ] ->
      Bit_zero_first'.of_string s
  | Sexp.List [ Sexp.Atom ("LE" | "B0L"); Sexp.Atom s ] | Sexp.Atom s ->
      Bit_zero_last'.of_string s
  | other -> Sexp_conv.of_sexp_error "not a bitvector" other

let sexp_of_t = Bit_zero_last'.sexp_of_t

module Bit_zero_first = struct
  include Bit_zero_first'

  let t_of_sexp = t_of_sexp
end

module Bit_zero_last = struct
  include Bit_zero_last'

  let t_of_sexp = t_of_sexp
end

let of_bool_iter iter =
  let result = ref (create ~len:0) in
  iter (fun bit ->
      let i = length !result in
      let new_result = extend_inplace !result ~len:(i + 1) in
      Unsafe.set_to new_result i bit;
      result := new_result);
  !result

let of_offset_iter iter =
  let result = ref (create ~len:0) in
  iter (fun i ->
      let new_result = extend_inplace !result ~len:(i + 1) in
      Unsafe.set new_result i;
      result := new_result);
  !result

let of_bool_seq seq = of_bool_iter (fun f -> Seq.iter f seq)
let of_offset_seq seq = of_offset_iter (fun f -> Seq.iter f seq)

let to_bool_seq v =
  let length = length v in
  let rec aux i () =
    if length > i then Seq.Cons (get v i, aux (i + 1)) else Seq.Nil
  in
  aux 0

let to_offset_seq v =
  let length = length v in
  let rec aux i () =
    if length > i then
      if get v i then Seq.Cons (i, aux (i + 1)) else aux (i + 1) ()
    else Seq.Nil
  in
  aux 0

let to_rev_bool_seq v =
  let length = length v in
  let rec aux i () =
    if i >= 0 then Seq.Cons (get v i, aux (i - 1)) else Seq.Nil
  in
  aux (length - 1)

let to_rev_offset_seq v =
  let length = length v in
  let rec aux i () =
    if i >= 0 then if get v i then Seq.Cons (i, aux (i - 1)) else aux (i - 1) ()
    else Seq.Nil
  in
  aux (length - 1)

module Builder = struct
  type t = {
    mutable current : int;
    mutable current_index : int;
    mutable list_len : int;
    mutable rev_list : int list;
  }

  let to_bitvector t =
    assert (Sys.int_size <= Element.bit_size);
    let len = (t.list_len * Sys.int_size) + t.current_index in
    let bv = create ~len in
    let list_len = t.list_len in
    let rec aux bv rev_list ~i =
      match rev_list with
      | [] -> ()
      | x :: xs ->
          let current_element_bit0 = (list_len - i - 1) * Sys.int_size in
          let _ =
            Unsafe.With_int.or_ ~result:bv bv ~bit0_at:current_element_bit0 x
          in
          aux bv xs ~i:(i + 1)
    in
    aux bv t.rev_list ~i:0;
    (if t.current_index > 0 then
       let current_element_bit0 = t.list_len * Sys.int_size in
       let _ =
         Unsafe.With_int.or_ ~result:bv bv ~bit0_at:current_element_bit0
           t.current
       in
       ());
    bv

  let create () =
    { current = 0; current_index = 0; list_len = 0; rev_list = [] }

  let reset t =
    t.current <- 0;
    t.current_index <- 0;
    t.rev_list <- [];
    t.list_len <- 0;
    ()

  let push t bit =
    let bit = (Obj.magic : bool -> int) bit in
    t.current <- t.current lor (bit lsl t.current_index);
    t.current_index <- succ t.current_index;
    if t.current_index = Sys.int_size then (
      t.rev_list <- t.current :: t.rev_list;
      t.current <- 0;
      t.current_index <- 0;
      t.list_len <- succ t.list_len)

  let add_iter builder iter =
    iter (fun bit -> push builder bit);
    builder

  let add_seq builder seq =
    Seq.fold_left
      (fun builder bit ->
        push builder bit;
        builder)
      builder seq

  let of_iter iter = add_iter (create ()) iter
  let of_seq seq = add_seq (create ()) seq
end
