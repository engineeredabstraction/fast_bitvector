open Core

let test_op1 ~op_bv ~op_bool bl =
  let bv = Fast_bitvector.Bit_zero_first.of_bool_list bl in
  let bl' = ListLabels.map bl ~f:op_bool in
  let bv' = op_bv bv in
  let bl'_as_bv = Fast_bitvector.Bit_zero_first.of_bool_list bl' in
  let eq_bv = Fast_bitvector.equal bv' bl'_as_bv in
  let eq_list = [%equal: bool list] bl' (Fast_bitvector.Bit_zero_first.to_bool_list bv') in
  eq_bv && eq_list

let test_op2 ~op_bv ~op_bool (bl1, bl2) =
  let bv1 = Fast_bitvector.Bit_zero_first.of_bool_list bl1 in
  let bv2 = Fast_bitvector.Bit_zero_first.of_bool_list bl2 in
  let bl' = ListLabels.map2 bl1 bl2 ~f:op_bool in
  let bv' = op_bv bv1 bv2 in
  let bl'_as_bv = Fast_bitvector.Bit_zero_first.of_bool_list bl' in
  let eq_bv = Fast_bitvector.equal bv' bl'_as_bv in
  let eq_list = [%equal: bool list] bl' (Fast_bitvector.Bit_zero_first.to_bool_list bv') in
  eq_bv && eq_list

let%test_unit "not" =
  QCheck.Test.make ~count:1000 ~name:"not" 
    QCheck.(list bool)
    (test_op1 ~op_bv:Fast_bitvector.Allocate.not ~op_bool:Stdlib.Bool.not)
  |> QCheck.Test.check_exn

let test_binary ~name ~op_bv ~op_bool =
  let g = 
    let open QCheck.Gen in
    let* len = nat in
    let* l1 = list_size (return len) bool in
    let+ l2 = list_size (return len) bool in
    (l1, l2)
  in
  let print a =
    [%sexp_of: (bool list) * (bool list)] a
    |> Sexp.to_string
  in
  let a = QCheck.make ~print g in
  QCheck.Test.make ~count:1000 ~name
    a
    (test_op2 ~op_bv ~op_bool)
  |> QCheck.Test.check_exn

let%test_unit "and" =
  test_binary ~name:"and" ~op_bv:Fast_bitvector.Allocate.and_ ~op_bool:(&&)

let%test_unit "or" =
  test_binary ~name:"or" ~op_bv:Fast_bitvector.Allocate.or_ ~op_bool:(||)

let%test_unit "xor" =
  test_binary ~name:"xor" ~op_bv:Fast_bitvector.Allocate.xor ~op_bool:Bool.(<>)

