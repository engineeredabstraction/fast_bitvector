
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

let count_trailing_zeros_32 (i : int32) : int =
  let x' = Int32.to_int i in
  let isolate = x' land (-x') in
  let deBruijn = 0x077CB531 in
  let product = (isolate * deBruijn) land 0xFFFFFFFF in
  let index = product lsr 27 in
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
