
let count_set_bits_64 (i : int64) : int =
  let open Int64 in
  let i = sub i (logand (shift_right_logical i 1) 0x5555555555555555L) in
  let i = add (logand i 0x3333333333333333L) (logand (shift_right_logical i 2) 0x3333333333333333L) in
  let i = logand (add i (shift_right_logical i 4)) 0x0F0F0F0F0F0F0F0FL in
  to_int (shift_right_logical (mul i 0x0101010101010101L) 56)

let count_set_bits_32 (i : int32) : int =
  let open Int32 in
  let i = sub i (logand (shift_right_logical i 1) 0x55555555l) in
  let i = add (logand i 0x33333333l) (logand (shift_right_logical i 2) 0x33333333l) in
  let i = logand (add i (shift_right_logical i 4)) 0x0F0F0F0Fl in
  to_int (shift_right_logical (mul i 0x01010101l) 24)

let ctz32_table = [|
  0; 1; 28; 2; 29; 14; 24; 3; 30; 22; 20; 15; 25; 17; 4; 8;
  31; 27; 13; 23; 21; 19; 16; 7; 26; 12; 18; 6; 11; 5; 10; 9
|]

let count_trailing_zeros_32 (x : int32) : int =
  let isolate = Int32.(logand x (neg x)) in
  let deBruijn = 0x077CB531l in
  let product = Int32.mul isolate deBruijn in
  let index = Int32.(to_int (shift_right_logical product 27)) in
  Array.unsafe_get ctz32_table index

let ctz64_table = [|
  63;  0; 58;  1; 59; 47; 53;  2;
  60; 39; 48; 27; 54; 33; 42;  3;
  61; 51; 37; 40; 49; 18; 28; 20;
  55; 30; 34; 11; 43; 14; 22;  4;
  62; 57; 46; 52; 38; 26; 32; 41;
  50; 36; 17; 19; 29; 10; 13; 21;
  56; 45; 25; 31; 35; 16;  9; 12;
  44; 24; 15;  8; 23;  7;  6;  5
|]

let count_trailing_zeros_64 (x : int64) : int =
  let isolated = Int64.(logand x (neg x)) in
  let deBruijn = 0x07EDD5E59A4E28C2L in
  let product = Int64.mul isolated deBruijn in
  let index = Int64.(to_int (shift_right_logical product 58)) in
  Array.unsafe_get ctz64_table index

let clz32_table = [|
  31; 22; 30; 21; 18; 10; 29;  2; 20; 17; 15; 13;  9;  6; 28;  1;
  23; 19; 11;  3; 12; 16; 14;  7; 24;  4;  8; 25;  5; 26; 27;  0
|]

let count_leading_zeros_32 (x : int32) : int =
  let x = Int32.logor x (Int32.shift_right_logical x 1) in
  let x = Int32.logor x (Int32.shift_right_logical x 2) in
  let x = Int32.logor x (Int32.shift_right_logical x 4) in
  let x = Int32.logor x (Int32.shift_right_logical x 8) in
  let x = Int32.logor x (Int32.shift_right_logical x 16) in
  let y = Int32.logand x (Int32.lognot (Int32.shift_right_logical x 1)) in
  let deBruijn = 0x07C4ACDDl in
  let product = Int32.mul y deBruijn in
  let index = Int32.to_int (Int32.shift_right_logical product 27) in
  Array.unsafe_get clz32_table index

let clz64_table = [|
  63;  5; 62;  4; 16; 10; 61;  3; 24; 15; 36;  9; 30; 21; 60;  2;
  12; 26; 23; 14; 45; 35; 43;  8; 33; 29; 52; 20; 58; 59;  1; 51;
  6; 13; 25; 44; 34; 32; 28; 57; 50;  7; 11; 22; 46; 42; 31; 27;
  53; 19; 56; 49; 54; 18; 48; 17; 47; 41; 55; 40; 39; 38; 37;  0
|]

let count_leading_zeros_64 (x : int64) : int =
  let x = Int64.logor x (Int64.shift_right_logical x 1) in
  let x = Int64.logor x (Int64.shift_right_logical x 2) in
  let x = Int64.logor x (Int64.shift_right_logical x 4) in
  let x = Int64.logor x (Int64.shift_right_logical x 8) in
  let x = Int64.logor x (Int64.shift_right_logical x 16) in
  let x = Int64.logor x (Int64.shift_right_logical x 32) in
  let y = Int64.logand x (Int64.lognot (Int64.shift_right_logical x 1)) in
  let deBruijn = 0x03F79D71B4CB0A89L in
  let product = Int64.mul y deBruijn in
  let index = Int64.to_int (Int64.shift_right_logical product 58) in
  Array.unsafe_get clz64_table index
