open! Expect_test_helpers_core
open! Sexplib0.Sexp_conv

let%expect_test "Basic" =
  let a, b = Fast_bitvector.create ~len:100, Fast_bitvector.create ~len:100 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect {|
    ((a (
       LE
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (b (
       LE
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)))
    |}];
  Fast_bitvector.set a 0;
  print_s [%message "" (a : Fast_bitvector.t)];
  [%expect {|
    (a (
      LE
      0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
    |}];
  Fast_bitvector.set a 99;
  print_s [%message "" (a : Fast_bitvector.t)];
  [%expect {|
    (a (
      LE
      1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
    |}];
  Fast_bitvector.not ~result:b a |> ignore;
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect {|
    ((a (
       LE
       1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
     (b (
       LE
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
       LE
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (b (
       LE
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
       LE
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110))
     (b (
       LE
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001)))
    |}];

  ()

let%expect_test "Popcount" =
  let a = Fast_bitvector.create ~len:100 in
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (a : Fast_bitvector.t) (pop : int)];
  [%expect {|
    ((a (
       LE
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (pop 0))
    |}];
  Fast_bitvector.set_all a;
  let pop = Fast_bitvector.popcount a in
  print_s [%message "" (a : Fast_bitvector.t) (pop : int)];
  [%expect {|
    ((a (
       LE
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (pop 100))
    |}]

let%expect_test "Append" =
  let a100, b100 = Fast_bitvector.create ~len:100, Fast_bitvector.create ~len:100 in
  Fast_bitvector.set_all a100;
  let c = Fast_bitvector.append a100 b100 in
  print_s [%message "" (a100 : Fast_bitvector.t) (b100 : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    ((a100 (
       LE
       1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111))
     (b100 (
       LE
       0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (c (
       LE
       00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)))
    |}]

let%expect_test "Extend" =
  let empty = Fast_bitvector.create ~len:0 in
  let empty_ten = Fast_bitvector.extend empty ~by:10 in
  print_s
    [%message
      ""
        (Fast_bitvector.is_empty empty : bool)
        (Fast_bitvector.is_empty empty_ten : bool)];
  [%expect
    {|
      (("Fast_bitvector.is_empty empty"     true)
       ("Fast_bitvector.is_empty empty_ten" true))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend a ~by:6 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 1111111111))
     (b (LE 0000001111111111)))
    |}];
  let a = Fast_bitvector.create_full ~len:65 in
  let b = Fast_bitvector.extend a ~by:1 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 11111111111111111111111111111111111111111111111111111111111111111))
     (b (LE 011111111111111111111111111111111111111111111111111111111111111111)))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend_inplace a ~by:6 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 0000001111111111))
     (b (LE 0000001111111111)))
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
  let _ = Fast_bitvector.and_ ~result:c a b in
  print_s [%message "and" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (and
      (a (LE 1111111111111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 1111111111000000000000000000000000000000)))
    |}];
  let _ = Fast_bitvector.or_ ~result:c a b in
  print_s [%message "or" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (or
      (a (LE 1111111111111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 1111111111111111111111111111110000000000)))
    |}];
  let _ = Fast_bitvector.xor ~result:c a b in
  print_s [%message "xor" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)
  ];
  [%expect {|
    (xor
      (a (LE 1111111111111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 0000000000111111111111111111110000000000)))
    |}]


let%expect_test "Conversion roundtrips" =
  let a, b, _ = init_mixed () in
  let a_iter =
    a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.of_iter
  in
  let a_seq = a |> Fast_bitvector.to_seq |> Fast_bitvector.of_seq in
  let b_iter =
    b |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.of_iter
  in
  let b_seq = b |> Fast_bitvector.to_seq |> Fast_bitvector.of_seq in
  print_s
    [%message
      "" (Fast_bitvector.equal a a_iter : bool) (Fast_bitvector.equal a a_seq : bool)
      (Fast_bitvector.equal b b_iter : bool) (Fast_bitvector.equal b b_seq : bool)
      (a : Fast_bitvector.t) (a_iter : Fast_bitvector.t) (a_seq : Fast_bitvector.t)
      (b : Fast_bitvector.t) (b_iter : Fast_bitvector.t) (b_seq : Fast_bitvector.t)
    ];
  [%expect
    {|
    (("Fast_bitvector.equal a a_iter" true)
     ("Fast_bitvector.equal a a_seq"  true)
     ("Fast_bitvector.equal b b_iter" true)
     ("Fast_bitvector.equal b b_seq"  true)
     (a      (LE 1111111111111111111100000000000000000000))
     (a_iter (LE 1111111111111111111100000000000000000000))
     (a_seq  (LE 1111111111111111111100000000000000000000))
     (b      (LE 1111111111000000000011111111110000000000))
     (b_iter (LE 1111111111000000000011111111110000000000))
     (b_seq  (LE 1111111111000000000011111111110000000000)))
    |}]

let%expect_test "Conversion roundtrips (long)" =
  let a, b, _ = init_mixed ~len:65 () in
  let a_iter =
    a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.of_iter
  in
  let a_seq = a |> Fast_bitvector.to_seq |> Fast_bitvector.of_seq in
  let b_iter =
    b |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.of_iter
  in
  let b_seq = b |> Fast_bitvector.to_seq |> Fast_bitvector.of_seq in
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
       LE
       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (a_iter (
       LE
       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (a_seq (
       LE
       11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
     (b (
       LE
       11111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000))
     (b_iter (
       LE
       11111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000))
     (b_seq (
       LE
       11111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000001111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000)))
    |}]

let%expect_test "builder" =
  let t = Fast_bitvector.Builder.create () in
  Fast_bitvector.Builder.push t true;
  print_s [%message "" (Fast_bitvector.Builder.to_bitvector t : Fast_bitvector.t)];
  [%expect {| ("Fast_bitvector.Builder.to_bitvector t" (LE 1)) |}];
  Fast_bitvector.Builder.push t false;
  print_s [%message "" (Fast_bitvector.Builder.to_bitvector t : Fast_bitvector.t)];
  [%expect {| ("Fast_bitvector.Builder.to_bitvector t" (LE 01)) |}];
  for i = 0 to 63 do
    Fast_bitvector.Builder.push t (i mod 2 = 0);
    let bv = Fast_bitvector.Builder.to_bitvector t in
    print_s [%message "" ~len:(Fast_bitvector.length bv : int) (bv : Fast_bitvector.t)];
  done;
  [%expect {|
    ((len 3) (bv (LE 101)))
    ((len 4) (bv (LE 0101)))
    ((len 5) (bv (LE 10101)))
    ((len 6) (bv (LE 010101)))
    ((len 7) (bv (LE 1010101)))
    ((len 8) (bv (LE 01010101)))
    ((len 9) (bv (LE 101010101)))
    ((len 10) (bv (LE 0101010101)))
    ((len 11) (bv (LE 10101010101)))
    ((len 12) (bv (LE 010101010101)))
    ((len 13) (bv (LE 1010101010101)))
    ((len 14) (bv (LE 01010101010101)))
    ((len 15) (bv (LE 101010101010101)))
    ((len 16) (bv (LE 0101010101010101)))
    ((len 17) (bv (LE 10101010101010101)))
    ((len 18) (bv (LE 010101010101010101)))
    ((len 19) (bv (LE 1010101010101010101)))
    ((len 20) (bv (LE 01010101010101010101)))
    ((len 21) (bv (LE 101010101010101010101)))
    ((len 22) (bv (LE 0101010101010101010101)))
    ((len 23) (bv (LE 10101010101010101010101)))
    ((len 24) (bv (LE 010101010101010101010101)))
    ((len 25) (bv (LE 1010101010101010101010101)))
    ((len 26) (bv (LE 01010101010101010101010101)))
    ((len 27) (bv (LE 101010101010101010101010101)))
    ((len 28) (bv (LE 0101010101010101010101010101)))
    ((len 29) (bv (LE 10101010101010101010101010101)))
    ((len 30) (bv (LE 010101010101010101010101010101)))
    ((len 31) (bv (LE 1010101010101010101010101010101)))
    ((len 32) (bv (LE 01010101010101010101010101010101)))
    ((len 33) (bv (LE 101010101010101010101010101010101)))
    ((len 34) (bv (LE 0101010101010101010101010101010101)))
    ((len 35) (bv (LE 10101010101010101010101010101010101)))
    ((len 36) (bv (LE 010101010101010101010101010101010101)))
    ((len 37) (bv (LE 1010101010101010101010101010101010101)))
    ((len 38) (bv (LE 01010101010101010101010101010101010101)))
    ((len 39) (bv (LE 101010101010101010101010101010101010101)))
    ((len 40) (bv (LE 0101010101010101010101010101010101010101)))
    ((len 41) (bv (LE 10101010101010101010101010101010101010101)))
    ((len 42) (bv (LE 010101010101010101010101010101010101010101)))
    ((len 43) (bv (LE 1010101010101010101010101010101010101010101)))
    ((len 44) (bv (LE 01010101010101010101010101010101010101010101)))
    ((len 45) (bv (LE 101010101010101010101010101010101010101010101)))
    ((len 46) (bv (LE 0101010101010101010101010101010101010101010101)))
    ((len 47) (bv (LE 10101010101010101010101010101010101010101010101)))
    ((len 48) (bv (LE 010101010101010101010101010101010101010101010101)))
    ((len 49) (bv (LE 1010101010101010101010101010101010101010101010101)))
    ((len 50) (bv (LE 01010101010101010101010101010101010101010101010101)))
    ((len 51) (bv (LE 101010101010101010101010101010101010101010101010101)))
    ((len 52) (bv (LE 0101010101010101010101010101010101010101010101010101)))
    ((len 53) (bv (LE 10101010101010101010101010101010101010101010101010101)))
    ((len 54) (bv (LE 010101010101010101010101010101010101010101010101010101)))
    ((len 55) (bv (LE 1010101010101010101010101010101010101010101010101010101)))
    ((len 56) (bv (LE 01010101010101010101010101010101010101010101010101010101)))
    ((len 57) (bv (LE 101010101010101010101010101010101010101010101010101010101)))
    ((len 58)
     (bv (LE 0101010101010101010101010101010101010101010101010101010101)))
    ((len 59)
     (bv (LE 10101010101010101010101010101010101010101010101010101010101)))
    ((len 60)
     (bv (LE 010101010101010101010101010101010101010101010101010101010101)))
    ((len 61)
     (bv (LE 1010101010101010101010101010101010101010101010101010101010101)))
    ((len 62)
     (bv (LE 01010101010101010101010101010101010101010101010101010101010101)))
    ((len 63)
     (bv (LE 101010101010101010101010101010101010101010101010101010101010101)))
    ((len 64)
     (bv (LE 0101010101010101010101010101010101010101010101010101010101010101)))
    ((len 65)
     (bv (LE 10101010101010101010101010101010101010101010101010101010101010101)))
    ((len 66)
     (bv (LE 010101010101010101010101010101010101010101010101010101010101010101)))
    |}]

