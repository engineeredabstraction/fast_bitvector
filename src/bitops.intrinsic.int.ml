
let [@inline always] count_set_bits_32 x =
  Ocaml_intrinsics_kernel.Int32.count_set_bits x

let [@inline always] count_set_bits_64 x =
  Ocaml_intrinsics_kernel.Int64.count_set_bits x

let [@inline always] count_trailing_zeros_32 x =
  Ocaml_intrinsics_kernel.Int32.count_trailing_zeros x

let [@inline always] count_trailing_zeros_64 x =
  Ocaml_intrinsics_kernel.Int64.count_trailing_zeros x
