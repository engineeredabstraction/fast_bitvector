open! Core

module Bitops = Fast_bitvector.Private.Bitops


let%expect_test _ =
  let v i =
    print_s 
      [%message ""
          ~i:(i : Int32.Hex.t)
          ~clz:(Bitops.count_leading_zeros_32 i : int)
          ~ctz:(Bitops.count_trailing_zeros_32 i : int)
      ]
  in
  v 0x1l;
  [%expect {| ((i 0x1) (clz 31) (ctz 0)) |}];
  v 0x2l;
  [%expect {| ((i 0x2) (clz 30) (ctz 1)) |}];
  v 0x4l;
  [%expect {| ((i 0x4) (clz 29) (ctz 2)) |}];
  v 0x8l;
  [%expect {| ((i 0x8) (clz 28) (ctz 3)) |}];
  v 0x10l;
  [%expect {| ((i 0x10) (clz 27) (ctz 4)) |}];
  v 0x20l;
  [%expect {| ((i 0x20) (clz 26) (ctz 5)) |}];
  v 0x40l;
  [%expect {| ((i 0x40) (clz 25) (ctz 6)) |}];
  v 0x80l;
  [%expect {| ((i 0x80) (clz 24) (ctz 7)) |}];
  v 0x100l;
  [%expect {| ((i 0x100) (clz 23) (ctz 8)) |}];
  v 0x200l;
  [%expect {| ((i 0x200) (clz 22) (ctz 9)) |}];
  v 0x400l;
  [%expect {| ((i 0x400) (clz 21) (ctz 10)) |}];
  v 0x800l;
  [%expect {| ((i 0x800) (clz 20) (ctz 11)) |}];
  v 0x1000l;
  [%expect {| ((i 0x1000) (clz 19) (ctz 12)) |}];
  v 0x2000l;
  [%expect {| ((i 0x2000) (clz 18) (ctz 13)) |}];
  v 0x4000l;
  [%expect {| ((i 0x4000) (clz 17) (ctz 14)) |}];
  v 0x8000l;
  [%expect {| ((i 0x8000) (clz 16) (ctz 15)) |}];
  v 0x1_0000l;
  [%expect {| ((i 0x10000) (clz 15) (ctz 16)) |}];
  v 0x2_0000l;
  [%expect {| ((i 0x20000) (clz 14) (ctz 17)) |}];
  v 0x4_0000l;
  [%expect {| ((i 0x40000) (clz 13) (ctz 18)) |}];
  v 0x8_0000l;
  [%expect {| ((i 0x80000) (clz 12) (ctz 19)) |}];
  v 0x10_0000l;
  [%expect {| ((i 0x100000) (clz 11) (ctz 20)) |}];
  v 0x20_0000l;
  [%expect {| ((i 0x200000) (clz 10) (ctz 21)) |}];
  v 0x40_0000l;
  [%expect {| ((i 0x400000) (clz 9) (ctz 22)) |}];
  v 0x80_0000l;
  [%expect {| ((i 0x800000) (clz 8) (ctz 23)) |}];
  v 0x100_0000l;
  [%expect {| ((i 0x1000000) (clz 7) (ctz 24)) |}];
  v 0x200_0000l;
  [%expect {| ((i 0x2000000) (clz 6) (ctz 25)) |}];
  v 0x400_0000l;
  [%expect {| ((i 0x4000000) (clz 5) (ctz 26)) |}];
  v 0x800_0000l;
  [%expect {| ((i 0x8000000) (clz 4) (ctz 27)) |}];
  v 0x1000_0000l;
  [%expect {| ((i 0x10000000) (clz 3) (ctz 28)) |}];
  v 0x2000_0000l;
  [%expect {| ((i 0x20000000) (clz 2) (ctz 29)) |}];
  v 0x4000_0000l;
  [%expect {| ((i 0x40000000) (clz 1) (ctz 30)) |}];
  v 0x8000_0000l;
  [%expect {| ((i -0x80000000) (clz 0) (ctz 31)) |}];
  ()

let%expect_test _ =
  let v i =
    print_s 
      [%message ""
          ~i:(i : Int64.Hex.t)
          ~clz:(Bitops.count_leading_zeros_64 i : int)
          ~ctz:(Bitops.count_trailing_zeros_64 i : int)
          ~total:(
            1 + (Bitops.count_leading_zeros_64 i) + (Bitops.count_trailing_zeros_64 i) : int)
      ]
  in
  v 0x1L;
  [%expect {| ((i 0x1) (clz 63) (ctz 0) (total 64)) |}];
  v 0x2L;
  [%expect {| ((i 0x2) (clz 62) (ctz 1) (total 64)) |}];
  v 0x4L;
  [%expect {| ((i 0x4) (clz 61) (ctz 2) (total 64)) |}];
  v 0x8L;
  [%expect {| ((i 0x8) (clz 60) (ctz 3) (total 64)) |}];
  v 0x10L;
  [%expect {| ((i 0x10) (clz 59) (ctz 4) (total 64)) |}];
  v 0x20L;
  [%expect {| ((i 0x20) (clz 58) (ctz 5) (total 64)) |}];
  v 0x40L;
  [%expect {| ((i 0x40) (clz 57) (ctz 6) (total 64)) |}];
  v 0x80L;
  [%expect {| ((i 0x80) (clz 56) (ctz 7) (total 64)) |}];
  v 0x100L;
  [%expect {| ((i 0x100) (clz 55) (ctz 8) (total 64)) |}];
  v 0x200L;
  [%expect {| ((i 0x200) (clz 54) (ctz 9) (total 64)) |}];
  v 0x400L;
  [%expect {| ((i 0x400) (clz 53) (ctz 10) (total 64)) |}];
  v 0x800L;
  [%expect {| ((i 0x800) (clz 52) (ctz 11) (total 64)) |}];
  v 0x1000L;
  [%expect {| ((i 0x1000) (clz 51) (ctz 12) (total 64)) |}];
  v 0x2000L;
  [%expect {| ((i 0x2000) (clz 50) (ctz 13) (total 64)) |}];
  v 0x4000L;
  [%expect {| ((i 0x4000) (clz 49) (ctz 14) (total 64)) |}];
  v 0x8000L;
  [%expect {| ((i 0x8000) (clz 48) (ctz 15) (total 64)) |}];
  v 0x1_0000L;
  [%expect {| ((i 0x10000) (clz 47) (ctz 16) (total 64)) |}];
  v 0x2_0000L;
  [%expect {| ((i 0x20000) (clz 46) (ctz 17) (total 64)) |}];
  v 0x4_0000L;
  [%expect {| ((i 0x40000) (clz 45) (ctz 18) (total 64)) |}];
  v 0x8_0000L;
  [%expect {| ((i 0x80000) (clz 44) (ctz 19) (total 64)) |}];
  v 0x10_0000L;
  [%expect {| ((i 0x100000) (clz 43) (ctz 20) (total 64)) |}];
  v 0x20_0000L;
  [%expect {| ((i 0x200000) (clz 42) (ctz 21) (total 64)) |}];
  v 0x40_0000L;
  [%expect {| ((i 0x400000) (clz 41) (ctz 22) (total 64)) |}];
  v 0x80_0000L;
  [%expect {| ((i 0x800000) (clz 40) (ctz 23) (total 64)) |}];
  v 0x100_0000L;
  [%expect {| ((i 0x1000000) (clz 39) (ctz 24) (total 64)) |}];
  v 0x200_0000L;
  [%expect {| ((i 0x2000000) (clz 38) (ctz 25) (total 64)) |}];
  v 0x400_0000L;
  [%expect {| ((i 0x4000000) (clz 37) (ctz 26) (total 64)) |}];
  v 0x800_0000L;
  [%expect {| ((i 0x8000000) (clz 36) (ctz 27) (total 64)) |}];
  v 0x1000_0000L;
  [%expect {| ((i 0x10000000) (clz 35) (ctz 28) (total 64)) |}];
  v 0x2000_0000L;
  [%expect {| ((i 0x20000000) (clz 34) (ctz 29) (total 64)) |}];
  v 0x4000_0000L;
  [%expect {| ((i 0x40000000) (clz 33) (ctz 30) (total 64)) |}];
  v 0x8000_0000L;
  [%expect {| ((i 0x80000000) (clz 32) (ctz 31) (total 64)) |}];

  v 0x1_0000_0000L;
  [%expect {| ((i 0x100000000) (clz 31) (ctz 32) (total 64)) |}];
  v 0x2_0000_0000L;
  [%expect {| ((i 0x200000000) (clz 30) (ctz 33) (total 64)) |}];
  v 0x4_0000_0000L;
  [%expect {| ((i 0x400000000) (clz 29) (ctz 34) (total 64)) |}];
  v 0x8_0000_0000L;
  [%expect {| ((i 0x800000000) (clz 28) (ctz 35) (total 64)) |}];
  v 0x10_0000_0000L;
  [%expect {| ((i 0x1000000000) (clz 27) (ctz 36) (total 64)) |}];
  v 0x20_0000_0000L;
  [%expect {| ((i 0x2000000000) (clz 26) (ctz 37) (total 64)) |}];
  v 0x40_0000_0000L;
  [%expect {| ((i 0x4000000000) (clz 25) (ctz 38) (total 64)) |}];
  v 0x80_0000_0000L;
  [%expect {| ((i 0x8000000000) (clz 24) (ctz 39) (total 64)) |}];
  v 0x100_0000_0000L;
  [%expect {| ((i 0x10000000000) (clz 23) (ctz 40) (total 64)) |}];
  v 0x200_0000_0000L;
  [%expect {| ((i 0x20000000000) (clz 22) (ctz 41) (total 64)) |}];
  v 0x400_0000_0000L;
  [%expect {| ((i 0x40000000000) (clz 21) (ctz 42) (total 64)) |}];
  v 0x800_0000_0000L;
  [%expect {| ((i 0x80000000000) (clz 20) (ctz 43) (total 64)) |}];
  v 0x1000_0000_0000L;
  [%expect {| ((i 0x100000000000) (clz 19) (ctz 44) (total 64)) |}];
  v 0x2000_0000_0000L;
  [%expect {| ((i 0x200000000000) (clz 18) (ctz 45) (total 64)) |}];
  v 0x4000_0000_0000L;
  [%expect {| ((i 0x400000000000) (clz 17) (ctz 46) (total 64)) |}];
  v 0x8000_0000_0000L;
  [%expect {| ((i 0x800000000000) (clz 16) (ctz 47) (total 64)) |}];
  v 0x1_0000_0000_0000L;
  [%expect {| ((i 0x1000000000000) (clz 15) (ctz 48) (total 64)) |}];
  v 0x2_0000_0000_0000L;
  [%expect {| ((i 0x2000000000000) (clz 14) (ctz 49) (total 64)) |}];
  v 0x4_0000_0000_0000L;
  [%expect {| ((i 0x4000000000000) (clz 13) (ctz 50) (total 64)) |}];
  v 0x8_0000_0000_0000L;
  [%expect {| ((i 0x8000000000000) (clz 12) (ctz 51) (total 64)) |}];
  v 0x10_0000_0000_0000L;
  [%expect {| ((i 0x10000000000000) (clz 11) (ctz 52) (total 64)) |}];
  v 0x20_0000_0000_0000L;
  [%expect {| ((i 0x20000000000000) (clz 10) (ctz 53) (total 64)) |}];
  v 0x40_0000_0000_0000L;
  [%expect {| ((i 0x40000000000000) (clz 9) (ctz 54) (total 64)) |}];
  v 0x80_0000_0000_0000L;
  [%expect {| ((i 0x80000000000000) (clz 8) (ctz 55) (total 64)) |}];
  v 0x100_0000_0000_0000L;
  [%expect {| ((i 0x100000000000000) (clz 7) (ctz 56) (total 64)) |}];
  v 0x200_0000_0000_0000L;
  [%expect {| ((i 0x200000000000000) (clz 6) (ctz 57) (total 64)) |}];
  v 0x400_0000_0000_0000L;
  [%expect {| ((i 0x400000000000000) (clz 5) (ctz 58) (total 64)) |}];
  v 0x800_0000_0000_0000L;
  [%expect {| ((i 0x800000000000000) (clz 4) (ctz 59) (total 64)) |}];
  v 0x1000_0000_0000_0000L;
  [%expect {| ((i 0x1000000000000000) (clz 3) (ctz 60) (total 64)) |}];
  v 0x2000_0000_0000_0000L;
  [%expect {| ((i 0x2000000000000000) (clz 2) (ctz 61) (total 64)) |}];
  v 0x4000_0000_0000_0000L;
  [%expect {| ((i 0x4000000000000000) (clz 1) (ctz 62) (total 64)) |}];
  v 0x8000_0000_0000_0000L;
  [%expect {| ((i -0x8000000000000000) (clz 0) (ctz 63) (total 64)) |}];
  ()
