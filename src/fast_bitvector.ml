

type t = Bytes.t

external get_int64 : bytes -> int -> int64 = "%caml_bytes_get64u"
external set_int64 : bytes -> int -> int64 -> unit = "%caml_bytes_set64u"

let length t =
  get_int64 t 0
  |> Int64.to_int

let word_size =
  assert (Sys.word_size = 64);
  Sys.word_size

let max_length =
  Sys.word_size * (Sys.max_string_length - 1)

let total_words ~length =
  (length + word_size - 1) lsr 6

let word_size_bytes = word_size / 8

let create ~length =
  let total_data_words = (length + word_size - 1) / word_size in
  let total_words = total_data_words + 1 in
  let t = Bytes.create (total_words * word_size_bytes) in
  set_int64 t 0 (Int64.of_int length);
  t


external (&&&) : bool -> bool -> bool = "%andint"

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
    let index = 1 + (i lsr 6) in
    Check.index t index;
    let subindex = i land 63 in
    let v = get_int64 t index in
    Int64.logand
      (Int64.shift_right v subindex)
      1L
    |> Int64.to_int
    |> (Obj.magic : int -> bool)

  let [@inline always] set t i =
    let index = 1 + (i lsr 6) in
    Check.index t index;
    let subindex = i land 63 in
    let v = get_int64 t index in
    let v' =
      Int64.logor v (Int64.shift_left 1L subindex)
    in
    set_int64 t index v'

  let [@inline always] set_to t i b =
    let b = Int64.of_int ((Obj.magic : bool -> int) b) in
    let index = 1 + (i lsr 6) in
    Check.index t index;
    let subindex = i land 63 in
    let v = get_int64 t index in
    let mask = Int64.lognot (Int64.shift_right v subindex) in
    let v' =
      Int64.logor
        (Int64.logand v mask)
        (Int64.shift_left b subindex)
    in
    set_int64 t index v'

  let [@inline always] clear t i =
    let index = 1 + (i lsr 6) in
    Check.index t index;
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

  let not a ~result =
    logop1 ~f:Int64.lognot a result

  let (land) a b ~result =
    logop2 ~f:Int64.logand a b result

  let (lor) a b ~result =
    logop2 ~f:Int64.logor a b result

  let (lxor) a b ~result =
    logop2 ~f:Int64.logxor a b result

end

module Unsafe = Ops(struct
    let index _ _ = ()
    let length2 a _ = length a
    let length3 a _ _ = length a
  end)

let init length ~f =
  let t = create ~length in
  for i = 0 to length - 1 do
    Unsafe.set_to t i ((f [@inlined hint]) i)
  done;
  t

let copy t =
  Bytes.copy t

let append_internal long short ~length_long ~length_short =
  (* CR smuenzel: only do individual sets on overlap, blit the rest *)
  let length = length_long + length_short in
  let t = create ~length in
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
  (init [@inlined always]) (length t)
    ~f:(fun i -> f (Unsafe.get t i))
