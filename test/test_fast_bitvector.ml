open! Expect_test_helpers_core
open! Sexplib0.Sexp_conv

let%expect_test "Basic" =
  let a, b = Fast_bitvector.create ~len:100, Fast_bitvector.create ~len:100 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect {|
    ((a (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (b (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
    |}];
  Fast_bitvector.set a 0;
  print_s [%message "" (a : Fast_bitvector.t)];
  [%expect {|
    (a (
      B0L
      0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
    |}];
  Fast_bitvector.set a 99;
  print_s [%message "" (a : Fast_bitvector.t)];
  [%expect {|
    (a (
      B0L
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
    |}];
  Fast_bitvector.not ~dst:b a;
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect {|
    ((a (
       B0L
       1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
     (b (
       B0L
       0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110)))
    |}];
  Fast_bitvector.set_all a;
  Fast_bitvector.clear_all b;
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)
      (Fast_bitvector.is_empty a : bool) (Fast_bitvector.is_empty b : bool)
      (Fast_bitvector.is_full a : bool) (Fast_bitvector.is_full b : bool)
  ];
  [%expect {|
    ((a (
       B0L
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (b (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     ("Fast_bitvector.is_empty a" false)
     ("Fast_bitvector.is_empty b" true)
     ("Fast_bitvector.is_full a"  true)
     ("Fast_bitvector.is_full b"  false))
    |}];
  Fast_bitvector.set_to a 0 false;
  Fast_bitvector.set_to b 0 true;
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect {|
    ((a (
       B0L
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110))
     (b (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)))
    |}];

  ()

let%expect_test "Popcount" =
  let a = Fast_bitvector.create ~len:100 in
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (a : Fast_bitvector.t) (pop : int)];
  [%expect {|
    ((a (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (pop 0))
    |}];
  Fast_bitvector.set_all a;
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (a : Fast_bitvector.t) (pop : int)];
  [%expect {|
    ((a (
       B0L
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (pop 100))
    |}];
  let a = Fast_bitvector.create ~len:100 in
  for i = 0 to 15 do 
    Fast_bitvector.set_to a i ((Int.rem i 2) = 0)
  done;
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (pop : int)];
  [%expect {| (pop 8) |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "0101101010101110101010" in
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (pop : int)];
  [%expect {| (pop 12) |}];
  let a = Fast_bitvector.create ~len:64 in
  Fast_bitvector.set a 63;
  Fast_bitvector.set a 62;
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (pop : int)];
  [%expect {| (pop 2) |}]

let%expect_test "Append" =
  let a100, b100 = Fast_bitvector.create ~len:100, Fast_bitvector.create ~len:100 in
  Fast_bitvector.set_all a100;
  let c = Fast_bitvector.append a100 b100 in
  print_s [%message "" (a100 : Fast_bitvector.t) (b100 : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    ((a100 (
       B0L
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (b100 (
       B0L
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (c (
       B0L
       00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)))
    |}]

let%expect_test "Extend" =
  let empty = Fast_bitvector.create ~len:0 in
  let empty_one = Fast_bitvector.extend empty ~len:1 in
  print_s
    [%message
      ""
        (Fast_bitvector.is_empty empty : bool)
        (Fast_bitvector.is_empty empty_one : bool)
        (Fast_bitvector.is_full empty : bool)
        (Fast_bitvector.is_full empty_one : bool)];
  [%expect
    {|
      (("Fast_bitvector.is_empty empty"     true)
       ("Fast_bitvector.is_empty empty_one" true)
       ("Fast_bitvector.is_full empty"      false)
       ("Fast_bitvector.is_full empty_one"  false))
    |}];
  let one = Fast_bitvector.extend_inplace empty ~len:1 in
  let _ = Fast_bitvector.set_all one in
  print_s
    [%message
      ""
        (Fast_bitvector.is_empty empty : bool)
        (Fast_bitvector.is_full one : bool)];
  [%expect
    {|
      (("Fast_bitvector.is_empty empty" true)
       ("Fast_bitvector.is_full one"    true))
    |}];
  let a = Fast_bitvector.create ~len:64 in
  Fast_bitvector.set a 45;
  Fast_bitvector.set a 49;
  Fast_bitvector.set a 63;
  print_s
    [%message
      ""
        (Fast_bitvector.is_empty a : bool)
        (Fast_bitvector.is_full a : bool)];
  [%expect
    {|
      (("Fast_bitvector.is_empty a" false)
       ("Fast_bitvector.is_full a"  false))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend a ~len:16 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.is_full a : bool)];
  [%expect
    {|
    ((a (B0L 1111111111))
     (b (B0L 0000001111111111))
     ("Fast_bitvector.is_full a" true))
    |}];
  let a = Fast_bitvector.create_full ~len:65 in
  let b = Fast_bitvector.extend a ~len:66 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (B0L 11111111111111111111111111111111111111111111111111111111111111111))
     (b (B0L 011111111111111111111111111111111111111111111111111111111111111111)))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend_inplace a ~len:16 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (B0L 0000001111111111))
     (b (B0L 0000001111111111)))
    |}];
  let a = [true;false;true;true;false;false;true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let b = Fast_bitvector.extend a ~len:16 in
  let c = b |> Fast_bitvector.to_offset_seq |> Fast_bitvector.of_offset_seq in
  print_s [%message "" (a : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect
    {|
    ((a (B0L 1001101))
     (c (B0L 1001101)))
    |}]

let init_mixed ?(len=10)() =
  let f () = Fast_bitvector.create ~len in
  let a0, a1, a2, a3 = f (), f (), f (), f () in
  let b0, b1, b2, b3 = f (), f (), f (), f () in
  let c = Fast_bitvector.create ~len:(4 * len) in
  Fast_bitvector.set_all a2;
  Fast_bitvector.set_all a3;
  Fast_bitvector.set_all b1;
  Fast_bitvector.set_all b3;
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a1 (Fast_bitvector.append a2 a3)) in
  let b = Fast_bitvector.append b0 (Fast_bitvector.append b1 (Fast_bitvector.append b2 b3)) in
  a, b, c

let%expect_test "Logical" = 
  let a, b, c = init_mixed () in
  Fast_bitvector.and_ ~dst:c a b;
  print_s [%message "and" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (and
      (a (B0L 1111111111111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 1111111111000000000000000000000000000000)))
    |}];
  Fast_bitvector.or_ ~dst:c a b;
  print_s [%message "or" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (or
      (a (B0L 1111111111111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 1111111111111111111111111111110000000000)))
    |}];
  Fast_bitvector.xor ~dst:c a b;
  print_s [%message "xor" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (xor
      (a (B0L 1111111111111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 0000000000111111111111111111110000000000)))
    |}];
  let empty = Fast_bitvector.create ~len:0 in
  let b = [false;true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let a = [true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let c = (Fast_bitvector.create ~len:2) in
  let _ = Fast_bitvector.Unsafe.inter ~dst:c a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal c empty : bool)];
  [%expect {|
    (equal
      (a (B0L 1))
      (b (B0L 10))
      ("Fast_bitvector.Relaxed.equal c empty" true))
    |}];
  let a = [false;true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let b = (Fast_bitvector.create ~len:2) in
  let _ = Fast_bitvector.Unsafe.or_ ~dst:b a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.equal a b : bool)];
  [%expect {|
    (equal
      (a (B0L 10))
      (b (B0L 10))
      ("Fast_bitvector.equal a b" true))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01110100101" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "01100100000" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "01010010000" in 
  print_s [%message "subset" (Fast_bitvector.subset b a: bool) (Fast_bitvector.subset c a: bool)];
  [%expect {|
    (subset
      ("Fast_bitvector.subset b a" true)
      ("Fast_bitvector.subset c a" false))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01011000100" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "00000110010" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "00110110010" in 
  print_s [%message "disjoint" (Fast_bitvector.disjoint b a: bool) (Fast_bitvector.disjoint a c: bool)];
  [%expect {|
    (disjoint 
      ("Fast_bitvector.disjoint b a" true)
      ("Fast_bitvector.disjoint a c" false))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01001101101" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "01101000100" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "11101010100" in 
  let m = Fast_bitvector.Bit_zero_first.of_string "11011010100" in 
  print_s [%message "modulo" (Fast_bitvector.equal_modulo ~modulo:m a b: bool) (Fast_bitvector.equal_modulo ~modulo:m a c: bool)];
  [%expect {|
    (modulo 
      ("Fast_bitvector.equal_modulo ~modulo:m a b" true)
      ("Fast_bitvector.equal_modulo ~modulo:m a c" false))
    |}]

let%expect_test "Convertion roundtrips" =
  let a = Fast_bitvector.create ~len:10 in
  for i = 0 to 9 do 
    Fast_bitvector.set_to a i ((Int.rem i 2) = 0)
  done;
  let b = a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector in
  let c = a |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let d = a |> Fast_bitvector.to_offset_seq |> Fast_bitvector.of_offset_seq in
  let e = a |> (fun t f -> Fast_bitvector.iter_seti ~f t) |> Fast_bitvector.of_offset_iter in
  print_s
    [%message
      "iters/seqs" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t) (d : Fast_bitvector.t) (e : Fast_bitvector.t)];
  [%expect
    {|
    (iters/seqs
      (a (B0L 0101010101))
      (b (B0L 0101010101))
      (c (B0L 0101010101))
      (d (B0L 101010101))
      (e (B0L 101010101)))
    |}];
  let b = a |> (fun t f -> Fast_bitvector.rev_iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector in
  let c = a |> (fun t f -> Fast_bitvector.rev_iter_seti ~f t) |> Fast_bitvector.of_offset_iter in
  let d = a |> Fast_bitvector.to_rev_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let e = a |> Fast_bitvector.to_rev_offset_seq |> Fast_bitvector.of_offset_seq in
  print_s
    [%message
      "reverse" (b : Fast_bitvector.t) (c : Fast_bitvector.t) (d : Fast_bitvector.t) (e : Fast_bitvector.t)];
  [%expect
    {|
    (reverse
      (b (B0L 1010101010))
      (c (B0L 101010101))
      (d (B0L 1010101010))
      (e (B0L 101010101)))
    |}];
    let b = a |> Fast_bitvector.Bit_zero_first.to_string |> Fast_bitvector.Bit_zero_first.of_string in
  print_s [%message "strings" (b : Fast_bitvector.t)];
  [%expect {| (strings (b (B0L 0101010101))) |}]

let%expect_test "Relaxed" = 
  let f () = Fast_bitvector.create ~len:10 in
  let a0, a1, a2, a3 = f (), f (), f (), f () in
  let b0, b1, b2, b3 = f (), f (), f (), f () in
  Fast_bitvector.set_all a2;
  Fast_bitvector.set_all a3;
  Fast_bitvector.set_all b1;
  Fast_bitvector.set_all b3;
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a1 a2) in
  let b = Fast_bitvector.append b0 (Fast_bitvector.append b1 (Fast_bitvector.append b2 b3)) in
  let c = Fast_bitvector.Relaxed.inter  a b in
  print_s [%message "inter" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (inter
      (a (B0L 111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 0000000000000000000000000000000000000000)))
    |}];
  let c = Fast_bitvector.Relaxed.inter   a b in
  print_s [%message "inter" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (inter
      (a (B0L 111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 0000000000000000000000000000000000000000)))
    |}];
  let c = Fast_bitvector.Relaxed.union   a b in
  print_s [%message "union" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (union
      (a (B0L 111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 1111111111111111111111111111110000000000)))
    |}];
  let c = Fast_bitvector.Relaxed.symmetric_diff   a b in
  print_s [%message "symdiff" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (symdiff
      (a (B0L 111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 1111111111111111111111111111110000000000)))
    |}];
  let c = Fast_bitvector.Relaxed.diff   a b in
  print_s [%message "diff" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (diff
      (a (B0L 111111111100000000000000000000))
      (b (B0L 1111111111000000000011111111110000000000))
      (c (B0L 0000000000111111111100000000000000000000)))
    |}];
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a2 a1) in
  let b = Fast_bitvector.append a0 a2 in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal a b : bool)];
  [%expect {|
    (equal
      (a (B0L 000000000011111111110000000000))
      (b (B0L 11111111110000000000))
      ("Fast_bitvector.Relaxed.equal a b" true))
    |}];
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a2 a1) in
  let b = Fast_bitvector.create ~len:0 in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal a b : bool)];
  [%expect {|
    (equal
      (a (B0L 000000000011111111110000000000))
      (b (B0L ""))
      ("Fast_bitvector.Relaxed.equal a b" false))
    |}];
  let empty = Fast_bitvector.create ~len:0 in
  let b = [false;true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let a = [true] |> List.to_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let c = Fast_bitvector.Relaxed.inter a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal c empty : bool)];
  [%expect {|
    (equal
      (a (B0L 1))
      (b (B0L 10))
      ("Fast_bitvector.Relaxed.equal c empty" true))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01110100101" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "0110010" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "010100100" in 
  print_s [%message "subset" (Fast_bitvector.Relaxed.subset b a: bool) (Fast_bitvector.Relaxed.subset c a: bool)];
  [%expect {|
    (subset
      ("Fast_bitvector.Relaxed.subset b a" true)
      ("Fast_bitvector.Relaxed.subset c a" false))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01011000" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "00000110010" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "00110110010" in 
  print_s [%message "disjoint" (Fast_bitvector.Relaxed.disjoint b a: bool) (Fast_bitvector.Relaxed.disjoint a c: bool)];
  [%expect {|
    (disjoint 
      ("Fast_bitvector.Relaxed.disjoint b a" true)
      ("Fast_bitvector.Relaxed.disjoint a c" false))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01001101101" in 
  let b = Fast_bitvector.Bit_zero_first.of_string "011010001" in 
  let c = Fast_bitvector.Bit_zero_first.of_string "101010100" in 
  let m = Fast_bitvector.Bit_zero_first.of_string "11011010100" in 
  print_s [%message "modulo" (Fast_bitvector.Relaxed.equal_modulo ~modulo:m a b: bool) (Fast_bitvector.Relaxed.equal_modulo ~modulo:m a c: bool)];
  [%expect {|
    (modulo 
      ("Fast_bitvector.Relaxed.equal_modulo ~modulo:m a b" true)
      ("Fast_bitvector.Relaxed.equal_modulo ~modulo:m a c" false))
    |}];
  let a = Fast_bitvector.Bit_zero_first.of_string "01110100101" in 
  print_s [%message "subset" (Fast_bitvector.Relaxed.mem a 0: bool) (Fast_bitvector.Relaxed.mem a 1: bool) (Fast_bitvector.Relaxed.mem a 20: bool)];
  [%expect {|
    (subset
      ("Fast_bitvector.Relaxed.mem a 0"  false)
      ("Fast_bitvector.Relaxed.mem a 1"  true)
      ("Fast_bitvector.Relaxed.mem a 20" false))
    |}]

let%expect_test "Conversion roundtrips (long)" =
  let a, b, _ = init_mixed ~len:30 () in
  let a_iter =
    a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector
  in
  let a_seq = a |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  let b_iter =
    b |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector
  in
  let b_seq = b |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in
  print_s
    [%message
      "" (Fast_bitvector.equal a a_iter : bool) (Fast_bitvector.equal a a_seq : bool)
      (Fast_bitvector.equal b b_iter : bool) (Fast_bitvector.equal b b_seq : bool)
      (a : Fast_bitvector.t) (a_iter : Fast_bitvector.t) (a_seq : Fast_bitvector.t)
      (b : Fast_bitvector.t) (b_iter : Fast_bitvector.t) (b_seq : Fast_bitvector.t)
    ];
  [%expect {|
    (("Fast_bitvector.equal a a_iter" true)
     ("Fast_bitvector.equal a a_seq"  true)
     ("Fast_bitvector.equal b b_iter" true)
     ("Fast_bitvector.equal b b_seq"  true)
     (a (
       B0L
       111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000))
     (a_iter (
       B0L
       111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000))
     (a_seq (
       B0L
       111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000))
     (b (
       B0L
       111111111111111111111111111111000000000000000000000000000000111111111111111111111111111111000000000000000000000000000000))
     (b_iter (
       B0L
       111111111111111111111111111111000000000000000000000000000000111111111111111111111111111111000000000000000000000000000000))
     (b_seq (
       B0L
       111111111111111111111111111111000000000000000000000000000000111111111111111111111111111111000000000000000000000000000000)))
    |}]

let%expect_test "builder" =
  let t = Fast_bitvector.Builder.create () in
  Fast_bitvector.Builder.push t true;
  print_s [%message "" (Fast_bitvector.Builder.to_bitvector t : Fast_bitvector.t)];
  [%expect {| ("Fast_bitvector.Builder.to_bitvector t" (B0L 1)) |}];
  Fast_bitvector.Builder.push t false;
  print_s [%message "" (Fast_bitvector.Builder.to_bitvector t : Fast_bitvector.t)];
  [%expect {| ("Fast_bitvector.Builder.to_bitvector t" (B0L 01)) |}];
  for i = 0 to 63 do
    Fast_bitvector.Builder.push t (i mod 2 = 0);
    let bv = Fast_bitvector.Builder.to_bitvector t in
    print_s [%message "" ~len:(Fast_bitvector.length bv : int)
        ~_:(bv : Fast_bitvector.Bit_zero_first.t)];
  done;
  [%expect {|
    ((len 3)
     (B0F 101))
    ((len 4)
     (B0F 1010))
    ((len 5)
     (B0F 10101))
    ((len 6)
     (B0F 101010))
    ((len 7)
     (B0F 1010101))
    ((len 8)
     (B0F 10101010))
    ((len 9)
     (B0F 101010101))
    ((len 10)
     (B0F 1010101010))
    ((len 11)
     (B0F 10101010101))
    ((len 12)
     (B0F 101010101010))
    ((len 13)
     (B0F 1010101010101))
    ((len 14)
     (B0F 10101010101010))
    ((len 15)
     (B0F 101010101010101))
    ((len 16)
     (B0F 1010101010101010))
    ((len 17)
     (B0F 10101010101010101))
    ((len 18)
     (B0F 101010101010101010))
    ((len 19)
     (B0F 1010101010101010101))
    ((len 20)
     (B0F 10101010101010101010))
    ((len 21)
     (B0F 101010101010101010101))
    ((len 22)
     (B0F 1010101010101010101010))
    ((len 23)
     (B0F 10101010101010101010101))
    ((len 24)
     (B0F 101010101010101010101010))
    ((len 25)
     (B0F 1010101010101010101010101))
    ((len 26)
     (B0F 10101010101010101010101010))
    ((len 27)
     (B0F 101010101010101010101010101))
    ((len 28)
     (B0F 1010101010101010101010101010))
    ((len 29)
     (B0F 10101010101010101010101010101))
    ((len 30)
     (B0F 101010101010101010101010101010))
    ((len 31)
     (B0F 1010101010101010101010101010101))
    ((len 32)
     (B0F 10101010101010101010101010101010))
    ((len 33)
     (B0F 101010101010101010101010101010101))
    ((len 34)
     (B0F 1010101010101010101010101010101010))
    ((len 35)
     (B0F 10101010101010101010101010101010101))
    ((len 36)
     (B0F 101010101010101010101010101010101010))
    ((len 37)
     (B0F 1010101010101010101010101010101010101))
    ((len 38)
     (B0F 10101010101010101010101010101010101010))
    ((len 39)
     (B0F 101010101010101010101010101010101010101))
    ((len 40)
     (B0F 1010101010101010101010101010101010101010))
    ((len 41)
     (B0F 10101010101010101010101010101010101010101))
    ((len 42)
     (B0F 101010101010101010101010101010101010101010))
    ((len 43)
     (B0F 1010101010101010101010101010101010101010101))
    ((len 44)
     (B0F 10101010101010101010101010101010101010101010))
    ((len 45)
     (B0F 101010101010101010101010101010101010101010101))
    ((len 46)
     (B0F 1010101010101010101010101010101010101010101010))
    ((len 47)
     (B0F 10101010101010101010101010101010101010101010101))
    ((len 48) (B0F 101010101010101010101010101010101010101010101010))
    ((len 49) (B0F 1010101010101010101010101010101010101010101010101))
    ((len 50) (B0F 10101010101010101010101010101010101010101010101010))
    ((len 51) (B0F 101010101010101010101010101010101010101010101010101))
    ((len 52) (B0F 1010101010101010101010101010101010101010101010101010))
    ((len 53) (B0F 10101010101010101010101010101010101010101010101010101))
    ((len 54) (B0F 101010101010101010101010101010101010101010101010101010))
    ((len 55) (B0F 1010101010101010101010101010101010101010101010101010101))
    ((len 56) (B0F 10101010101010101010101010101010101010101010101010101010))
    ((len 57) (B0F 101010101010101010101010101010101010101010101010101010101))
    ((len 58) (B0F 1010101010101010101010101010101010101010101010101010101010))
    ((len 59) (B0F 10101010101010101010101010101010101010101010101010101010101))
    ((len 60) (B0F 101010101010101010101010101010101010101010101010101010101010))
    ((len 61) (B0F 1010101010101010101010101010101010101010101010101010101010101))
    ((len 62)
     (B0F 10101010101010101010101010101010101010101010101010101010101010))
    ((len 63)
     (B0F 101010101010101010101010101010101010101010101010101010101010101))
    ((len 64)
     (B0F 1010101010101010101010101010101010101010101010101010101010101010))
    ((len 65)
     (B0F 10101010101010101010101010101010101010101010101010101010101010101))
    ((len 66)
     (B0F 101010101010101010101010101010101010101010101010101010101010101010))
    |}]

let%expect_test "Convertion roundtrips (builder)" =
  let a = Fast_bitvector.create ~len:10 in
  for i = 0 to 9 do 
    Fast_bitvector.set_to a i ((Int.rem i 2) = 0)
  done;
  let b = a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector in
  let c = a |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in 
  print_s
    [%message
      "iters/seqs" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t) ];
  [%expect
    {|
    (iters/seqs
      (a (B0L 0101010101))
      (b (B0L 0101010101))
      (c (B0L 0101010101)))
    |}];
  let a, b, _ = init_mixed ~len:30 () in
  let a_iter = a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector in
  let a_seq = a |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in 
  let b_iter = b |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.Builder.of_iter |> Fast_bitvector.Builder.to_bitvector in
  let b_seq = b |> Fast_bitvector.to_bool_seq |> Fast_bitvector.Builder.of_seq |> Fast_bitvector.Builder.to_bitvector in 
  print_s
    [%message
      "" (Fast_bitvector.equal a a_iter : bool) (Fast_bitvector.equal a a_seq : bool)
      (Fast_bitvector.equal b b_iter : bool) (Fast_bitvector.equal b b_seq : bool)
    ];
  [%expect {|
    (("Fast_bitvector.equal a a_iter" true)
     ("Fast_bitvector.equal a a_seq"  true)
     ("Fast_bitvector.equal b b_iter" true)
     ("Fast_bitvector.equal b b_seq"  true))
    |} ]

let%expect_test "Logical(big)" = 
  let f () = Fast_bitvector.create ~len:20 in
  let a0, a1, a2, a3 = f (), f (), f (), f () in
  let b0, b1, b2, b3 = f (), f (), f (), f () in
  let c = Fast_bitvector.create ~len:80 in
  Fast_bitvector.set_all a2;
  Fast_bitvector.set_all a3;
  Fast_bitvector.set_all b1;
  Fast_bitvector.set_all b3;
  let a23 = Fast_bitvector.append a2 a3 in
  let a123 = Fast_bitvector.append a1 a23 in
  let a = Fast_bitvector.append a0 a123 in
  print_s [%message "append" (a0 : Fast_bitvector.t) (a1 : Fast_bitvector.t) (a2 : Fast_bitvector.t) (a3 : Fast_bitvector.t) (a23 : Fast_bitvector.t) (a123 : Fast_bitvector.t) (a : Fast_bitvector.t)];
  [%expect {|
    (append
      (a0  (B0L 00000000000000000000))
      (a1  (B0L 00000000000000000000))
      (a2  (B0L 11111111111111111111))
      (a3  (B0L 11111111111111111111))
      (a23 (B0L 1111111111111111111111111111111111111111))
      (a123 (B0L 111111111111111111111111111111111111111100000000000000000000))
      (a (
        B0L
        11111111111111111111111111111111111111110000000000000000000000000000000000000000)))
    |}];
  let b = Fast_bitvector.append b0 (Fast_bitvector.append b1 (Fast_bitvector.append b2 b3)) in
  ();
  Fast_bitvector.and_ ~dst:c a b;
  print_s [%message "and" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (and
      (a (
        B0L
        11111111111111111111111111111111111111110000000000000000000000000000000000000000))
      (b (
        B0L
        11111111111111111111000000000000000000001111111111111111111100000000000000000000))
      (c (
        B0L
        11111111111111111111000000000000000000000000000000000000000000000000000000000000)))
    |}];
  Fast_bitvector.or_ ~dst:c a b;
  print_s [%message "or" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (or
      (a (
        B0L
        11111111111111111111111111111111111111110000000000000000000000000000000000000000))
      (b (
        B0L
        11111111111111111111000000000000000000001111111111111111111100000000000000000000))
      (c (
        B0L
        11111111111111111111111111111111111111111111111111111111111100000000000000000000)))
    |}];
  Fast_bitvector.xor ~dst:c a b;
  print_s [%message "xor" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (xor
      (a (
        B0L
        11111111111111111111111111111111111111110000000000000000000000000000000000000000))
      (b (
        B0L
        11111111111111111111000000000000000000001111111111111111111100000000000000000000))
      (c (
        B0L
        00000000000000000000111111111111111111111111111111111111111100000000000000000000)))
    |}]
