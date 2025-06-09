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
    |}]

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

let%expect_test "Logical" = 
  let f () = Fast_bitvector.create ~len:10 in
  let a0, a1, a2, a3 = f (), f (), f (), f () in
  let b0, b1, b2, b3 = f (), f (), f (), f () in
  let c = Fast_bitvector.create ~len:40 in
  Fast_bitvector.set_all a2;
  Fast_bitvector.set_all a3;
  Fast_bitvector.set_all b1;
  Fast_bitvector.set_all b3;
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a1 (Fast_bitvector.append a2 a3)) in
  let b = Fast_bitvector.append b0 (Fast_bitvector.append b1 (Fast_bitvector.append b2 b3)) in
  ();
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
    |}]


