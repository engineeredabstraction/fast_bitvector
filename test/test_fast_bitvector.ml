open! Core
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

let%test_unit "randomize" =
  if Stdlib.Sys.word_size = 32
  then begin
    (* Skip for now, until we have CI infra for 32bit *)
    ()
  end else begin
    let version = Stdlib.Sys.ocaml_release in
    let random1, random2 =
      if version.major < 5
      then begin
        "1010001111011010110111111010011010111100010110011100110111111000010000000001001110011110011111010000000101011010111001011000011010010010110001"
      , "1110001001011111111110001110000110111100101001101011010101111110011001011000010111011111111101011011010111110000011111001110010111011110101110"
      end else begin
        "1100011111001001110010101111010000111101101101001111001101011111101010111111100000000101011111010111001001011111100111011010101000011111101011"
      , "1101110100000100101000110001011110110010110100001111000100110001100110010111000001000101110000111001110111111110101001100011001100001100101011"
      end
    in
    let t = Fast_bitvector.create ~len:(64*2+14) in
    Random.init 0;
    Fast_bitvector.randomize t;
    let s1 = Fast_bitvector.Bit_zero_last.to_string t in
    Fast_bitvector.randomize t;
    let s2 = Fast_bitvector.Bit_zero_last.to_string t in
    [%test_result: string] s1 ~expect:random1;
    [%test_result: string] s2 ~expect:random2
  end
