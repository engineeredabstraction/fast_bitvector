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
  let empty_ten = Fast_bitvector.extend empty ~len:1 in
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
  let empty_ten = Fast_bitvector.extend_inplace empty ~len:1 in
  let _ = Fast_bitvector.set_all empty_ten in
  print_s
    [%message
      ""
        (Fast_bitvector.is_empty empty : bool)
        (Fast_bitvector.is_full empty_ten : bool)];
  [%expect
    {|
      (("Fast_bitvector.is_empty empty"    true)
       ("Fast_bitvector.is_full empty_ten" true))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend a ~len:16 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 1111111111))
     (b (LE 0000001111111111)))
    |}];
  let a = Fast_bitvector.create_full ~len:65 in
  let b = Fast_bitvector.extend a ~len:66 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 11111111111111111111111111111111111111111111111111111111111111111))
     (b (LE 011111111111111111111111111111111111111111111111111111111111111111)))
    |}];
  let a = Fast_bitvector.create_full ~len:10 in
  let b = Fast_bitvector.extend_inplace a ~len:16 in
  print_s [%message "" (a : Fast_bitvector.t) (b : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 0000001111111111))
     (b (LE 0000001111111111)))
    |}];
  let a = [true;false;true;true;false;false;true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let b = Fast_bitvector.extend a ~len:16 in
  let c = b |> Fast_bitvector.to_offset_seq |> Fast_bitvector.of_offset_seq in
  print_s [%message "" (a : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect
    {|
    ((a (LE 1001101))
     (c (LE 1001101)))
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
    |}];
  let empty = Fast_bitvector.create ~len:0 in
  let b = [false;true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let a = [true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let c = Fast_bitvector.Unsafe.Set.intersect ~result:(Fast_bitvector.create ~len:2) a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal c empty : bool)];
  [%expect {|
    (equal
      (a (LE 1))
      (b (LE 10))
      ("Fast_bitvector.Relaxed.equal c empty" true))
    |}];
  let a = [false;true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let b = (Fast_bitvector.create ~len:2) in
  let b = Fast_bitvector.Unsafe.or_ ~result:b a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal a b : bool)];
  [%expect {|
    (equal
      (a (LE 10))
      (b (LE 10))
      ("Fast_bitvector.Relaxed.equal a b" true))
    |}]

let%expect_test "Convertion roundtrips" =
  let a = Fast_bitvector.create ~len:10 in
  for i = 0 to 9 do 
    Fast_bitvector.set_to a i ((Int.rem i 2) = 0)
  done;
  let b = a |> (fun t f -> Fast_bitvector.iter ~f t) |> Fast_bitvector.of_bool_iter in
  let c = a |> Fast_bitvector.to_bool_seq |> Fast_bitvector.of_bool_seq in
  let d = a |> Fast_bitvector.to_offset_seq |> Fast_bitvector.of_offset_seq in
  let e = a |> (fun t f -> Fast_bitvector.iter_seti ~f t) |> Fast_bitvector.of_offset_iter in
  print_s
    [%message
      "" (Fast_bitvector.Relaxed.equal a b : bool) (Fast_bitvector.Relaxed.equal a c : bool) (Fast_bitvector.Relaxed.equal a d : bool) (Fast_bitvector.Relaxed.equal a e : bool)];
  [%expect
    {|
    (("Fast_bitvector.Relaxed.equal a b" true)
     ("Fast_bitvector.Relaxed.equal a c" true)
     ("Fast_bitvector.Relaxed.equal a d" true)
     ("Fast_bitvector.Relaxed.equal a e" true))
    |}]

let%expect_test "Relaxed" = 
  let f () = Fast_bitvector.create ~len:10 in
  let a0, a1, a2, a3 = f (), f (), f (), f () in
  let b0, b1, b2, b3 = f (), f (), f (), f () in
  let c = Fast_bitvector.create ~len:40 in
  let d = Fast_bitvector.create ~len:25 in
  Fast_bitvector.set_all a2;
  Fast_bitvector.set_all a3;
  Fast_bitvector.set_all b1;
  Fast_bitvector.set_all b3;
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a1 a2) in
  let b = Fast_bitvector.append b0 (Fast_bitvector.append b1 (Fast_bitvector.append b2 b3)) in
  let _ = Fast_bitvector.Relaxed.intersect ~result:c a b in
  print_s [%message "inter" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (inter
      (a (LE 111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 0000000000000000000000000000000000000000)))
    |}];
  let _ = Fast_bitvector.Relaxed.intersect ~result:d a b in
  print_s [%message "inter" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (inter
      (a (LE 111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 0000000000000000000000000000000000000000)))
    |}];
  let _ = Fast_bitvector.Relaxed.union ~result:c a b in
  print_s [%message "union" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (union
      (a (LE 111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 1111111111111111111111111111110000000000)))
    |}];
  let _ = Fast_bitvector.Relaxed.symmetric_difference ~result:c a b in
  print_s [%message "symdiff" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (symdiff
      (a (LE 111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 1111111111111111111111111111110000000000)))
    |}];
  let _ = Fast_bitvector.Relaxed.difference ~result:c a b in
  print_s [%message "diff" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (c : Fast_bitvector.t)];
  [%expect {|
    (diff
      (a (LE 111111111100000000000000000000))
      (b (LE 1111111111000000000011111111110000000000))
      (c (LE 0000000000111111111100000000000000000000)))
    |}];
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a2 a1) in
  let b = Fast_bitvector.append a0 a2 in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal a b : bool)];
  [%expect {|
    (equal
      (a (LE 000000000011111111110000000000))
      (b (LE 11111111110000000000))
      ("Fast_bitvector.Relaxed.equal a b" true))
    |}];
  let a = Fast_bitvector.append a0 (Fast_bitvector.append a2 a1) in
  let b = Fast_bitvector.create ~len:0 in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal a b : bool)];
  [%expect {|
    (equal
      (a (LE 000000000011111111110000000000))
      (b (LE ""))
      ("Fast_bitvector.Relaxed.equal a b" false))
    |}];
  let empty = Fast_bitvector.create ~len:0 in
  let b = [false;true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let a = [true] |> List.to_seq |> Fast_bitvector.of_bool_seq in
  let c = Fast_bitvector.Relaxed.intersect ~result:(Fast_bitvector.create ~len:2) a b in
  print_s [%message "equal" (a : Fast_bitvector.t) (b : Fast_bitvector.t) (Fast_bitvector.Relaxed.equal c empty : bool)];
  [%expect {|
    (equal
      (a (LE 1))
      (b (LE 10))
      ("Fast_bitvector.Relaxed.equal c empty" true))
    |}]

