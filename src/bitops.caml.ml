
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

let ctz32_table =
  "\000\001\028\002\029\014\024\003\030\022\020\015\025\017\004\008\031\027\013\023\021\019\016\007\026\012\018\006\011\005\010\009"

let count_trailing_zeros_32 (x : int32) : int =
  let isolate = Int32.(logand x (neg x)) in
  let deBruijn = 0x077CB531l in
  let product = Int32.mul isolate deBruijn in
  let index = Int32.(to_int (shift_right_logical product 27)) in
  String.unsafe_get ctz32_table index
  |> Char.code

let ctz64_table =
 "\063\000\058\001\059\047\053\002\060\039\048\027\054\033\042\003\061\051\037\040\049\018\028\020\055\030\034\011\043\014\022\004\062\057\046\052\038\026\032\041\050\036\017\019\029\010\013\021\056\045\025\031\035\016\009\012\044\024\015\008\023\007\006\005"

let count_trailing_zeros_64 (x : int64) : int =
  let isolated = Int64.(logand x (neg x)) in
  let deBruijn = 0x07EDD5E59A4E28C2L in
  let product = Int64.mul isolated deBruijn in
  let index = Int64.(to_int (shift_right_logical product 58)) in
  String.unsafe_get ctz64_table index
  |> Char.code

let clz32_table =
 "\031\030\021\029\020\017\009\028\001\019\016\014\012\008\005\027\000\022\018\010\002\015\013\006\023\011\003\007\024\004\025\026"

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
  String.unsafe_get clz32_table index
  |> Char.code

let clz64_table =
 "\063\062\015\061\006\014\035\060\002\005\013\021\025\034\046\059\001\008\004\027\010\012\020\041\018\024\030\033\039\045\051\058\000\016\007\036\003\022\026\047\009\028\011\042\019\031\040\052\017\037\023\048\029\043\032\053\038\049\044\054\050\055\056\057"

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
  String.unsafe_get clz64_table index
  |> Char.code
