(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

let (=) = Int.equal

type t = Bytes.t

let failwithf s = Printf.ksprintf failwith s

module type Element = sig
  type t

  val to_string : t -> string

  val bit_size : int
  val byte_size : int
  val shift : int
  val index_mask : int

  val equal : t -> t -> bool

  val random : unit -> t

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
  val count_trailing_zeros : t -> int
  val count_leading_zeros : t -> int
end

module Element_32 = struct
  include Int32

  let to_string = Printf.sprintf "0x%08lx"

  let bit_size = 32
  let byte_size = 4
  let shift = 5
  let index_mask = 31

  let random = Random.bits32

  external get : bytes -> int -> t = "%caml_bytes_get32u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set32u"

  let set b i v = set b (i*byte_size) v
  let get b i = get b (i*byte_size)

  let count_set_bits = Bitops.count_set_bits_32
  let count_trailing_zeros = Bitops.count_trailing_zeros_32
  let count_leading_zeros = Bitops.count_leading_zeros_32

  let of_int i =
    logand (of_int i) max_int
end

module Element_64 = struct
  include Int64

  let to_string = Printf.sprintf "0x%016Lx"

  let bit_size = 64
  let byte_size = 8
  let shift = 6
  let index_mask = 63

  let random = Random.bits64

  external get : bytes -> int -> t = "%caml_bytes_get64u"
  external set : bytes -> int -> t -> unit = "%caml_bytes_set64u"

  let set b i v = set b (i*byte_size) v
  let get b i = get b (i*byte_size)

  let count_set_bits = Bitops.count_set_bits_64
  let count_trailing_zeros = Bitops.count_trailing_zeros_64
  let count_leading_zeros = Bitops.count_leading_zeros_64

  let of_int i =
    logand (of_int i) max_int
end

module Element = (val
                   if Sys.word_size = 32
                    then (module Element_32 : Element)
                    else (module Element_64 : Element)
                 )

let _ = Element.to_string

let length t =
  Element.get t 0
  |> Element.to_int

let max_length =
  ((Sys.max_string_length / Element.byte_size) - 1) * Element.bit_size

let [@inline always] total_data_words ~length =
  (length + Element.bit_size - 1) lsr Element.shift

let [@inline always] full_data_words ~length =
  (length lsr Element.shift)

let [@inline always] total_words ~length =
  1 + (total_data_words ~length)

let create_internal ~new_length ~init =
  if new_length > max_length
  then failwithf "length %d exceeds maximum length %d" new_length max_length;
  let total_words = total_words ~length:new_length in
  let t = Bytes.make (total_words * Element.byte_size) init in
  Element.set t 0 (Element.of_int new_length);
  assert (length t == new_length);
  t

let create ~len:new_length = create_internal ~new_length ~init:'\000'

let [@inline always] loop_set result value =
  let length = length result in
  let total_data_words = total_data_words ~length in
  for i = 1 to total_data_words do
    Element.set result i
      value
  done

let set_all t =
  loop_set t Element.minus_one

let clear_all t =
  loop_set t Element.zero

let randomize t =
  let length = length t in
  let full_data_words = full_data_words ~length in
  let total_data_words = total_data_words ~length in
  for i = 1 to full_data_words do
    Element.set t i
      (Element.random ())
  done;
  if total_data_words > full_data_words
  then begin
    let mask = Element.(sub (shift_left one (length mod bit_size)) one) in
    Element.set t total_data_words
      (Element.logand
         mask
         (Element.random ())
      )
  end

external (&&&) : bool -> bool -> bool = "%andint"
external (|||) : bool -> bool -> bool = "%orint"

let [@inline always] foldop1 ~init ~f ~final a =
  let length = length a in
  let total_data_words = total_data_words ~length in
  let acc = ref init in
  for i = 1 to pred total_data_words do
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
      (Element.get a total_data_words)
  end else begin
    let mask = Element.(sub (shift_left one remaining) one) in
    (f [@inlined hint])
      !acc
      (final ~mask (Element.get a total_data_words))
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
  val wrap_int : (t' -> bit0_at:int -> int -> t' -> unit) -> (t' -> bit0_at:int -> int -> _ t)
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

  let [@inline always] wrap_int (f : t' -> bit0_at:int -> int -> t' -> unit) : (t' -> bit0_at:int -> int -> 'a t) =
    fun bv ~bit0_at int ~dst ->
      (f [@inlined hint]) bv ~bit0_at int dst
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

  let [@inline always] wrap_int (f : t' -> bit0_at:int -> int -> t' -> unit) : (t' -> bit0_at:int -> int -> 'a t) =
    fun bv ~bit0_at int ->
      let res_bv = create ~len:(length bv) in
      f bv ~bit0_at int res_bv;
      res_bv
end

module [@inline always] Ops(Check : Check)(Make_result : Make_result) = struct
  let [@inline always] logop1 ~f =
    let [@inline always] inner_f a result =
      let length = Check.length2 a result in
      let total_data_words = total_data_words ~length in
      let full_data_words = full_data_words ~length in
      for i = 1 to full_data_words do
        Element.set result i
          (f
             (Element.get a i)
          )
      done;
      if total_data_words > full_data_words
      then begin
        let remaining = length land Element.index_mask in
        let mask = Element.(sub (shift_left one remaining) one) in
        Element.set result total_data_words
          (Element.logand
             mask
             (f (Element.get a total_data_words))
          )
      end
    in
    Make_result.wrap_1 inner_f

  let [@inline always] logop2 ~f =
    let [@inline always] inner_f a b result =
      let length = Check.length3 a b result in
      let total_data_words = total_data_words ~length in
      let full_data_words = full_data_words ~length in
      for i = 1 to full_data_words do
        Element.set result i
          (f
             (Element.get a i)
             (Element.get b i)
          )
      done;
      if total_data_words > full_data_words
      then begin
        let remaining = length land Element.index_mask in
        let mask = Element.(sub (shift_left one remaining) one) in
        Element.set result total_data_words
          (Element.logand
             mask
             (f (Element.get a total_data_words) (Element.get b total_data_words))
          )
      end
    in
    Make_result.wrap_2 inner_f

  let [@inline always] foldop2 ~init ~f ~final a b =
    let length = Check.length2 a b in
    let total_data_words = total_data_words ~length in
    let acc = ref init in
    for i = 1 to pred total_data_words do
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
      (final ~mask (Element.get a total_data_words))
      (final ~mask (Element.get b total_data_words))

  let [@inline always] foldop3 ~init ~f ~final a b c =
    let length = Check.length3 a b c in
    let total_words = total_words ~length in
    let acc = ref init in
    for i = 1 to pred total_words do
      acc :=
        (f [@inlined hint])
          !acc
          (Element.get a i)
          (Element.get b i)
          (Element.get c i)
    done;
    let remaining = length land (Element.bit_size - 1) in
    let mask = Element.sub (Element.shift_left Element.one remaining) Element.one in
    (f [@inlined hint])
      !acc
      (final ~mask (Element.get a total_words))
      (final ~mask (Element.get b total_words))
      (final ~mask (Element.get c total_words))

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

  let equal_modulo ~modulo a b =
    foldop3 modulo a b
      ~init:true
      ~f:(fun acc modulo a b ->
          acc
          &&&
          Element.(equal zero (logand modulo (logxor a b))))
      ~final:(fun ~mask a -> Element.logand mask a)

  let [@inline always] not a = logop1 ~f:Element.lognot a

  let [@inline always] and_ a b = logop2 ~f:Element.logand a b

  let [@inline always] nand a b =
    logop2 ~f:(fun a b ->
        Element.lognot (Element.logand a b)
      ) a b

  let [@inline always] or_ a b = logop2 ~f:Element.logor a b

  let [@inline always] nor a b =
    logop2 ~f:(fun a b ->
        Element.lognot (Element.logor a b)
      ) a b

  let [@inline always] xor a b = logop2 ~f:Element.logxor a b

  let [@inline always] xnor a b =
    logop2 ~f:(fun a b ->
        Element.lognot (Element.logxor a b)
      ) a b

  module Set = struct
    let mem = get

    let inter = and_
    let complement = not
    let symmetric_diff = xor
    let union = or_
    let cardinality = popcount

    let [@inline always] diff a b =
      logop2 ~f:(fun a b ->
          Element.logand a (Element.lognot b)
        ) a b

    let are_disjoint a b =
      foldop2 a b
        ~init:true
        ~f:(fun acc a b ->
            acc
            &&&
            (Element.equal Element.zero (Element.logand a b))
          )
        ~final:(fun ~mask a -> Element.logand mask a)

    let is_subset ~of_ a =
      foldop2 a of_
        ~init:true
        ~f:(fun acc a of_ ->
            acc
            &&&
            (Element.equal Element.zero (Element.logand a (Element.lognot of_)))
          )
        ~final:(fun ~mask a -> Element.logand mask a)

  end

  module With_int = struct
    let or_ =
      let[@inline always] inner_f a ~bit0_at b_int result =
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
        end
      in
      Make_result.wrap_int inner_f
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

let equal_modulo ~modulo a b =
  let la = length a in
  let lb = length b in
  let lm = length modulo in
  if la = lb && la = lm
  then Unsafe.equal_modulo ~modulo a b
  else false

let init new_length ~f =
  let t = create ~len:new_length in
  for i = 0 to new_length - 1 do
    Unsafe.set_to t i ((f [@inlined hint]) i);
  done;
  t

let create_full ~len:new_length = create_internal ~new_length ~init:'\xFF'

let copy t =
  Bytes.copy t

module Bitblit = struct
  let loop ~src ~src_pos ~dst ~dst_pos ~len =
    for i = 0 to pred len do
      Unsafe.set_to dst (dst_pos + i) (Unsafe.get src (src_pos + i))
    done

  let element_merge ~src ~src_pos ~dst ~dst_element_pos ~len_elements =
    let src_element_pos = 1 + (src_pos lsr Element.shift) in
    let src_element_offset = src_pos land Element.index_mask in
    if src_element_offset = 0
    then begin
      for i = 0 to pred len_elements do
        let src_element = Element.get src (src_element_pos + i) in
        Element.set dst (1 + dst_element_pos + i) src_element
      done
    end else begin
      for i = 0 to pred len_elements do
        let src_element0 = Element.get src (src_element_pos + i) in
        let src_element1 = Element.get src (1 + src_element_pos + i) in
        let dst_element = 
          Element.logor
            (Element.shift_right_logical src_element0 src_element_offset)
            (Element.shift_left src_element1 (Element.bit_size - src_element_offset))
        in
        Element.set dst (1 + dst_element_pos + i) dst_element
      done
    end

  let bitblit ~src ~src_pos ~dst ~dst_pos ~len =
    let length_src = length src in
    let length_dst = length dst in
    if length_src - src_pos < len
    then failwithf "bitblit: source too short: %d < %d" (length_src - src_pos) len;
    if length_dst - dst_pos < len
    then failwithf "bitblit: destination too short: %d < %d" (length_dst - dst_pos) len;
    (* First, loop until we can write full elements in the destination, then clean
       up any remainder *)
    let first_loop_start = dst_pos in
    let full_loop_element_start = ((dst_pos + Element.bit_size -1) lsr Element.shift) in
    let full_loop_bit_start = full_loop_element_start * Element.bit_size in
    let full_loop_element_stop = ((dst_pos + len - 1) lsr Element.shift) in
    let full_loop_element_length = Int.max 0 (full_loop_element_stop - full_loop_element_start) in
    let full_loop_bit_stop = full_loop_element_stop * Element.bit_size in
    let full_loop_bit_length = Int.max 0 (full_loop_bit_stop - full_loop_bit_start) in
    let first_loop_stop = Int.min full_loop_bit_start (dst_pos + len) in
    let first_loop_length = Int.max 0 (first_loop_stop - first_loop_start) in
    let remaining_length = Int.max 0 (len - (first_loop_length + full_loop_bit_length)) in
    let last_loop_start = full_loop_bit_stop in
    if len <> first_loop_length + full_loop_bit_length + remaining_length
    then
      failwithf
        "bitblit: length mismatch, len=%d, first_loop_length=%d, full_loop_bit_length=%d, remaining_length=%d"
        len first_loop_length full_loop_bit_length remaining_length;
    loop ~src ~src_pos ~dst ~dst_pos ~len:first_loop_length;
    element_merge
      ~src ~src_pos:(src_pos + first_loop_length)
      ~dst ~dst_element_pos:full_loop_element_start
      ~len_elements:full_loop_element_length;
    loop
      ~src ~src_pos:(src_pos + first_loop_length + full_loop_bit_length)
      ~dst ~dst_pos:last_loop_start
      ~len:remaining_length
end


let append a b =
  let length_a = length a in
  let length_b = length b in
  let length = length_a + length_b in
  let words_a = total_data_words ~length:length_a in
  let t = create ~len:length in
  for i = 1 to words_a do
    Element.set t i (Element.get a i)
  done;
  Bitblit.bitblit ~src:b ~dst:t ~src_pos:0 ~dst_pos:length_a ~len:length_b;
  t 

open Sexplib0

module type Bit_ordering_spec = sig
  val sexp_name : string
  val get_index : length:int -> i:int -> int
end

module Bit_ordering_conversion(Spec : Bit_ordering_spec) = struct
  module Spec = Spec

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

  let of_bool_list l =
    let len = List.length l in
    let t = create ~len in
    ListLabels.iteri l ~f:(fun i b ->
        Unsafe.set_to t (Spec.get_index ~length:len ~i) b
      );
    t

  let to_bool_list t =
    let len = length t in
    List.init len (fun i ->
        Unsafe.get t (Spec.get_index ~length:len ~i)
      )
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

module type Fold_ordering_spec = sig
  val get_index : length:int -> i:int -> int
  
  type ('a, 'b) fold_f
  type ('a, 'b) foldi_f

  val apply_fold_f : ('a, 'b) fold_f -> 'b -> 'a -> 'a
  val apply_foldi_f : ('a, 'b) foldi_f -> int -> 'b -> 'a -> 'a

  val zeros_until_next_bit : Element.t -> int
  val skip_next_bit : Element.t -> int -> Element.t

end

module Fold_ordering = struct
  (* For fold_left *)
  module Bit_zero_first = struct
    include Bit_zero_first'.Spec

    type ('a, 'b) fold_f = 'a -> 'b -> 'a
    type ('a, 'b) foldi_f = 'a -> int -> 'b -> 'a

    let [@inline always] apply_fold_f f b acc = f acc b
    let [@inline always] apply_foldi_f f i b acc = f acc i b

    let zeros_until_next_bit e = Element.count_trailing_zeros e
    let skip_next_bit = Element.shift_right_logical
  end

  (* For fold_right *)
  module Bit_zero_last = struct
    include Bit_zero_last'.Spec

    type ('a, 'b) fold_f = 'b -> 'a -> 'a
    type ('a, 'b) foldi_f = int -> 'b -> 'a -> 'a

    let [@inline always] apply_fold_f f b acc = f b acc
    let [@inline always] apply_foldi_f f i b acc = f i b acc

    let zeros_until_next_bit e = Element.count_leading_zeros e
    let skip_next_bit = Element.shift_left
  end
end

module [@inline always] Basic_fold(Spec : Fold_ordering_spec) = struct
  let [@inline always] foldi t ~init ~f =
    let length = length t in
    let acc = ref init in
    for i = 0 to pred length do
      acc := f !acc i (Unsafe.get t (Spec.get_index ~length ~i))
    done;
    !acc

  let [@inline always] fold t ~init ~f =
    foldi t ~init ~f:(fun acc _i b -> f acc b)

  let [@inline always] foldi t ~init ~f =
    foldi t ~init ~f:(fun acc i b -> Spec.apply_foldi_f f i b acc)

  let [@inline always] fold t ~init ~f =
    fold t ~init ~f:(fun acc b -> Spec.apply_fold_f f b acc)

  let [@inline always] iteri t ~f =
    let length = length t in
    for i = 0 to pred length do
      f i (Unsafe.get t (Spec.get_index ~length ~i))
    done

  let [@inline always] iter t ~f =
    iteri t ~f:(fun _i b -> f b)

  (* Only consider set bits *)
  let [@inline always] fold_set t ~init ~f =
    let length = length t in
    let total_data_words = total_data_words ~length in
    let acc = ref init in
    for i = 1 to total_data_words do
      let i' = Spec.get_index ~length:total_data_words ~i:(i - 1) + 1 in
      let word = ref (Element.get t i') in
      let subindex = ref 0 in
      while Bool.not (Element.equal !word Element.zero) do
        let tail = Spec.zeros_until_next_bit !word in
        subindex := !subindex + tail;
        word := Spec.skip_next_bit !word tail;
        word := Spec.skip_next_bit !word 1;
        let subindex' = Spec.get_index ~length:Element.bit_size ~i:!subindex in
        let user_index = (i' - 1) * Element.bit_size + subindex' in
        acc := Spec.apply_fold_f f user_index !acc;
        incr subindex;
      done
    done;
    !acc

end

module Fold_forward = struct 
  include Basic_fold(Fold_ordering.Bit_zero_first)

  let [@inline always] iter_set t ~f =
    fold_set t ~init:() ~f:(fun () i -> f i)
end

module Fold_backward = struct
  include Basic_fold(Fold_ordering.Bit_zero_last)

  let [@inline always] iter_set t ~f =
    fold_set t ~init:() ~f:(fun i () -> f i)
end

let fold = Fold_forward.fold
let fold_left = Fold_forward.fold
let foldi = Fold_forward.foldi
let fold_lefti = Fold_forward.foldi
let iter = Fold_forward.iter
let iteri = Fold_forward.iteri
let fold_set = Fold_forward.fold_set
let fold_left_set = Fold_forward.fold_set
let iter_set = Fold_forward.iter_set

let fold_right = Fold_backward.fold
let fold_righti = Fold_backward.foldi
let rev_iter = Fold_backward.iter
let rev_iteri = Fold_backward.iteri
let rev_iter_set = Fold_backward.iter_set
let fold_right_set = Fold_backward.fold_set

let map t ~f =
  (init [@inlined hint]) (length t)
    ~f:(fun i -> f (Unsafe.get t i))

let mapi t ~f =
  (init [@inlined hint]) (length t)
    ~f:(fun i -> f i (Unsafe.get t i))


module Builder = struct
  type t =
    { mutable current : int
    ; mutable current_index : int
    ; mutable list_len : int
    ; mutable rev_list : int list
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
        Unsafe.With_int.or_ ~dst:bv bv ~bit0_at:current_element_bit0 x;
        aux bv xs ~i:(i + 1)
    in
    aux bv t.rev_list ~i:0;
    if t.current_index > 0
    then begin
      let current_element_bit0 = t.list_len * Sys.int_size in
      Unsafe.With_int.or_ ~dst:bv bv ~bit0_at:current_element_bit0 t.current
    end;
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
    if t.current_index = Sys.int_size
    then begin
      t.rev_list <- t.current :: t.rev_list;
      t.current <- 0;
      t.current_index <- 0;
      t.list_len <- succ t.list_len
    end

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

module Private = struct
  module Bitops = Bitops
end
