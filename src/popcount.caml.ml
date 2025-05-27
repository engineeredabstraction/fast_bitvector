
let count_set_bits (i : int64) : int =
  let open Int64 in
  let i = sub i (logand (shift_right_logical i 1) 0x5555555555555555L) in
  let i = add (logand i 0x3333333333333333L) (logand (shift_right_logical i 2) 0x3333333333333333L) in
  let i = logand (add i (shift_right_logical i 4)) 0x0F0F0F0F0F0F0F0FL in
  to_int (shift_right_logical (mul i 0x0101010101010101L) 56)

