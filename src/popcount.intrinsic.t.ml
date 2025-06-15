
let [@inline always] count_set_bits_32 x =
  Ocaml_intrinsics_kernel.Int32.count_set_bits x
  |> Int32.to_int

let [@inline always] count_set_bits_64 x =
  Ocaml_intrinsics_kernel.Int64.count_set_bits x
  |> Int64.to_int
