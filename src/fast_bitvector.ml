(* SPDX-License-Identifier: MPL-2.0
 * SPDX-FileCopyrightText: (c) 2025 Stefan Muenzel
 *)

type t = Bytes.t

let failwithf s = Printf.ksprintf failwith s

external get_int64 : bytes -> int -> int64 = "%caml_bytes_get64u"
external set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

let set_int64 b i v = set_int64 b (i*8) v
let get_int64 b i = get_int64 b (i*8)

let length t =
  get_int64 t 0
  |> Int64.to_int

let word_size =
  assert (Sys.word_size = 64);
  Sys.word_size

let max_length =
  (Sys.max_string_length*8) - word_size

let total_words ~length =
  (length + word_size - 1) lsr 6

let word_size_bytes = word_size / 8

let create ~len:new_length =
  if new_length > max_length
  then failwithf "length %d exceeds maximum length %d" new_length max_length;
  let total_data_words = (new_length + word_size - 1) / word_size in
  let total_words = total_data_words + 1 in
  let t = Bytes.init (total_words * word_size_bytes) (fun _ -> '\x00') in
  set_int64 t 0 (Int64.of_int new_length);
  assert (length t == new_length);
  t

let [@inline always] loop_set result value =
  let length = length result in
  let total_words = total_words ~length in
  for i = 1 to total_words do
    set_int64 result i
      value
  done

let set_all t =
  loop_set t Int64.minus_one

let clear_all t =
  loop_set t Int64.zero

external (&&&) : bool -> bool -> bool = "%andint"

let [@inline always] foldop1 ~init ~f ~final a =
  let length = length a in
  let total_words = total_words ~length in
  let acc = ref init in
  for i = 1 to pred total_words do
    acc :=
      (f [@inlined hint])
        !acc
        (get_int64 a i)
  done;
  let remaining = length land 63 in
  let mask = Int64.sub (Int64.shift_left 1L remaining) 1L in
  (f [@inlined hint])
    !acc
    (final ~mask (get_int64 a total_words))

let popcount t =
  foldop1 t 
    ~init:0
    ~f:(fun acc v -> acc + (Popcount64.count_set_bits v))
    ~final:(fun ~mask a -> Int64.logand mask a)

let is_empty t =
  foldop1 t
    ~init:true
    ~f:(fun acc v -> acc &&& (v = Int64.zero))
    ~final:(fun ~mask a -> Int64.logand mask a)

let is_full t =
  foldop1 t
    ~init:true
    ~f:(fun acc v -> acc &&& (v = Int64.minus_one))
    ~final:(fun ~mask a -> Int64.logor (Int64.lognot mask) a)
    
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
      set_int64 result i
        (f
           (get_int64 a i)
        )
    done;
    result

  let [@inline always] logop2 ~f a b result =
    let length = Check.length3 a b result in
    let total_words = total_words ~length in
    for i = 1 to total_words do
      set_int64 result i
        (f
           (get_int64 a i)
           (get_int64 b i)
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
          (get_int64 a i)
          (get_int64 b i)
    done;
    let remaining = length land 63 in
    let mask = Int64.sub (Int64.shift_left 1L remaining) 1L in
    (f [@inlined hint])
      !acc
      (final ~mask (get_int64 a total_words))
      (final ~mask (get_int64 b total_words))

  let [@inline always] get t i =
    Check.index t i;
    let index = 1 + (i lsr 6) in
    let subindex = i land 63 in
    let v = get_int64 t index in
    Int64.logand
      (Int64.shift_right_logical v subindex)
      1L
    |> Int64.to_int
    |> (Obj.magic : int -> bool)

  let [@inline always] set t i =
    Check.index t i;
    let index = 1 + (i lsr 6) in
    let subindex = i land 63 in
    let v = get_int64 t index in
    let v' =
      Int64.logor v (Int64.shift_left 1L subindex)
    in
    set_int64 t index v'

  let [@inline always] set_to t i b =
    Check.index t i;
    let b = Int64.of_int ((Obj.magic : bool -> int) b) in
    let index = 1 + (i lsr 6) in
    let subindex = i land 63 in
    let v = get_int64 t index in
    let mask = Int64.lognot (Int64.shift_left Int64.one subindex) in
    let v' =
      Int64.logor
        (Int64.logand v mask)
        (Int64.shift_left b subindex)
    in
    set_int64 t index v'

  let [@inline always] clear t i =
    Check.index t i;
    let index = 1 + (i lsr 6) in
    let subindex = i land 63 in
    let v = get_int64 t index in
    let v' =
      Int64.logand v (Int64.lognot (Int64.shift_left 1L subindex))
    in
    set_int64 t index v'

  let equal a b =
    foldop2 a b
      ~init:true
      ~f:(fun acc a b ->
          acc
          &&&
          (0L = (Int64.logxor a b))
        )
      ~final:(fun ~mask a -> Int64.logand mask a)

  let not ~result a =
    logop1 ~f:Int64.lognot a result

  let and_ ~result a b =
    logop2 ~f:Int64.logand a b result

  let or_ ~result a b =
    logop2 ~f:Int64.logor a b result

  let xor ~result a b =
    logop2 ~f:Int64.logxor a b result

  module Set = struct
    let mem = get

    let intersect = and_
    let complement = not
    let symmetric_difference = xor

    let difference ~result a b =
      logop2 ~f:(fun a b ->
          Int64.logand a (Int64.lognot b)
        ) a b result
  end

end

module Unsafe = Ops(struct
    let index _ _ = ()
    let length2 a _ = length a
    let length3 a _ _ = length a
  end)

include Ops(struct
    let index t i = assert (0 <= i && i < length t)

    let length2 a b =
      let la = length a in
      let lb = length b in
      assert (la = lb);
      la

    let length3 a b c =
      let la = length a in
      let lb = length b in
      let lc = length c in
      assert (la = lb);
      assert (la = lc);
      la
    end)

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

let copy t =
  Bytes.copy t

let append_internal long short ~length_long ~length_short =
  (* CR smuenzel: only do individual sets on overlap, blit the rest *)
  let length = length_long + length_short in
  let t = create ~len:length in
  Bytes.blit long 1 t 1 (length / word_size_bytes);
  for i = 0 to pred length_short do
    Unsafe.set_to t (length_long + i) (Unsafe.get short i)
  done;
  t

let append a b =
  let length_a = length a in
  let length_b = length b in
  if length_a >= length_b
  then append_internal a b ~length_long:length_a ~length_short:length_b
  else append_internal b a ~length_long:length_b ~length_short:length_a

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
