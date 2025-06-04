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
  val index_mask : int

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
  let index_mask = 31

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
  let index_mask = 63

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
  let remaining = length land (Element.bit_size - 1) in
  let mask = Element.sub (Element.shift_left Element.one remaining) Element.one in
  (f [@inlined hint])
    !acc
    (final ~mask (Element.get a total_words))

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

module [@inline always] Ops(Check : Check) = struct
  let [@inline always] logop1 ~f a result =
    let length = Check.length2 a result in
    let total_words = total_words ~length in
    for i = 1 to total_words do
      Element.set result i
        (f
           (Element.get a i)
        )
    done;
    result

  let [@inline always] logop2 ~f a b result =
    let length = Check.length3 a b result in
    let total_words = total_words ~length in
    for i = 1 to total_words do
      Element.set result i
        (f
           (Element.get a i)
           (Element.get b i)
        )
    done;
    result

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
    foldop2 a b
      ~init:true
      ~f:(fun acc a b ->
          acc
          &&&
          (Element.equal Element.zero (Element.logxor a b))
        )
      ~final:(fun ~mask a -> Element.logand mask a)

  let not ~result a =
    logop1 ~f:Element.lognot a result

  let and_ ~result a b =
    logop2 ~f:Element.logand a b result

  let or_ ~result a b =
    logop2 ~f:Element.logor a b result

  let xor ~result a b =
    logop2 ~f:Element.logxor a b result

  module Set = struct
    let mem = get

    let intersect = and_
    let complement = not
    let symmetric_difference = xor

    let difference ~result a b =
      logop2 ~f:(fun a b ->
          Element.logand a (Element.lognot b)
        ) a b result
    
    let union = or_
  end

  module With_int = struct
    let or_ ~result a ~bit0_at b_int =
      Check.index a bit0_at;
      let b_el = Element.of_int b_int in
      let bit0_element_index = 1 + (bit0_at lsr Element.shift) in
      let bitN_element_index = 1 + ((bit0_at + Sys.int_size - 1) lsr Element.shift) in
      let should_set_N = (bitN_element_index - 1) * Element.bit_size < length a in
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
      let v' =
        Element.logor v (Element.shift_left b_el element0_shift_left)
      in
      Element.set result bit0_element_index v';
      if bit0_element_index <> bitN_element_index && should_set_N
      then begin
        let v = Element.get a bitN_element_index in
        let v' =
          Element.logor v (Element.shift_right_logical b_el elementN_shift_right)
        in
        Element.set result bitN_element_index v'
      end;
      result
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

module Unsafe = Ops(Check_none)

include Ops(Check_all)

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
  Unsafe.not ~result:t t

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

let copy_bits src dst =
  let length = Int.min (length src) (length dst) in
  let byte_size = (length + 7) / 8 in
  if byte_size > 1 then (
    (* This assumes that bit direction in Element and bytes is the same, I guess? *)
    Bytes.blit src Element.byte_size dst Element.byte_size (byte_size - 1);
    for i = 8 * (length / 8) to pred length do
      Unsafe.set_to dst i (Unsafe.get src i)
    done)

let extend ~by v =
  let len = length v + by in
  let new_vec = create ~len in
  copy_bits v new_vec;
  new_vec

let extend_inplace ~by v =
  let prev_length = length v in
  let new_length = prev_length + by in
  let prev_capacity = 8 * Bytes.length v in
  let new_vec =
    if new_length <= prev_capacity then (
      Element.set v 0 (Element.of_int new_length);
      v)
    else extend ~by v
  in
  for i = prev_length to pred prev_capacity do
    Unsafe.set_to new_vec i false
  done;
  new_vec

let[@inline always] foldi ~init ~f t =
  let length = length t in
  let acc = ref init in
  for i = 0 to pred length do
    (* CR smuenzel: process word at a time *)
    acc := f !acc i (Unsafe.get t i)
  done;
  !acc

let fold ~init ~f v = foldi ~init ~f:(fun acc _i b -> f acc b) v

let map t ~f =
  (init [@inlined hint]) (length t) ~f:(fun i -> f (Unsafe.get t i))

let mapi t ~f =
  (init [@inlined hint]) (length t) ~f:(fun i -> f i (Unsafe.get t i))

open Sexplib0

module Big_endian' = struct
  type nonrec t = t

  let to_string t =
    let length = length t in
    (String.init [@inlined hint]) length (fun i ->
        if Unsafe.get t i
        then '1'
        else '0'
      )

  let of_string s =
    let length = String.length s in
    init length
      ~f:(fun i ->
          match String.unsafe_get s i with
          | '0' -> false
          | '1' -> true
          | other ->
            failwithf "invalid char '%c'" other
        )

  let sexp_of_t t =
    Sexp.List
      [ Sexp.Atom "BE"
      ; Sexp.Atom (to_string t)
      ]
end

module Little_endian' = struct
  type nonrec t = t

  let to_string t =
    let length = length t in
    (String.init [@inlined hint]) length (fun i ->
        if Unsafe.get t (length - (i + 1))
        then '1'
        else '0'
      )

  let of_string s =
    let length = String.length s in
    let result =
      init length
        ~f:(fun i ->
            match String.get s (length - (i + 1)) with
            | '0' -> false
            | '1' -> true
            | other ->
              failwithf "invalid char '%c'" other
          )
    in
    result

  let sexp_of_t t =
    Sexp.List
      [ Sexp.Atom "LE"
      ; Sexp.Atom (to_string t)
      ]
end

let t_of_sexp = function
  | Sexp.List
      [ Sexp.Atom "BE"
      ; Sexp.Atom s
      ] -> Big_endian'.of_string s
  | Sexp.List
      [ Sexp.Atom "LE"
      ; Sexp.Atom s
      ]
  | Sexp.Atom s ->
    Little_endian'.of_string s
  | other ->
    Sexp_conv.of_sexp_error "not a bitvector" other

let sexp_of_t = Little_endian'.sexp_of_t

module Big_endian = struct
  include Big_endian'

  let t_of_sexp = t_of_sexp
end

module Little_endian = struct
  include Little_endian'

  let t_of_sexp = t_of_sexp
end

let iteri ~f v = foldi ~init:() ~f:(fun _ i bit -> f i bit) v

let iter ~f v = iteri ~f:(fun _i b -> f b) v

let to_seq v =
  let rec aux v i () =
    if length v > i then Seq.Cons (get v i, aux v (i + 1)) else Seq.Nil
  in
  aux v 0

module Builder = struct
  type t =
    { mutable current : int
    ; mutable current_index : int
    ; mutable list_len : int
    ; mutable rev_list : int list
    }

  let to_bitvector t =
    assert (Sys.int_size <= Element.bit_size);
    let len = t.list_len * Sys.int_size + t.current_index in
    let bv = create ~len in
    let list_len = t.list_len in
    let rec aux bv rev_list ~i =
      match rev_list with
      | [] -> ()
      | x :: xs ->
        let current_element_bit0 = (list_len - i - 1) * Sys.int_size in
        let _ = Unsafe.With_int.or_ ~result:bv bv ~bit0_at:current_element_bit0 x in
        aux bv xs ~i:(i + 1)
    in
    aux bv t.rev_list ~i:0;
    if t.current_index > 0
    then begin
      let current_element_bit0 = t.list_len * Sys.int_size in
      let _ = Unsafe.With_int.or_ ~result:bv bv ~bit0_at:current_element_bit0 t.current in
      ()
    end;
    bv

  let create () =
    { current = 0
    ; current_index = 0
    ; list_len = 0
    ; rev_list = []
    }

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
    if t.current_index = Sys.int_size 
    then begin
      t.rev_list <- t.current :: t.rev_list;
      t.current <- 0;
      t.current_index <- 0;
      t.list_len <- succ t.list_len
    end

end

let of_iter iter =
  let builder = Builder.create () in
  iter (fun bit -> Builder.push builder bit);
  Builder.to_bitvector builder

let of_seq seq =
  Seq.fold_left
    (fun builder bit -> Builder.push builder bit; builder)
    (Builder.create ())
    seq
  |> Builder.to_bitvector
