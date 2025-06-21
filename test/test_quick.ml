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

module Gen = struct
  let list_len = QCheck.Gen.int_range 0 (64*3 + 20)

  let list_pair = 
    let open QCheck.Gen in
    let* len = list_len in
    let* l1 = list_size (return len) bool in
    let+ l2 = list_size (return len) bool in
    (l1, l2)
end

module Arbitrary = struct
  let list = QCheck.list_of_size Gen.list_len QCheck.bool

  let list_pair =
    let print a =
      [%sexp_of: (bool list) * (bool list)] a
      |> Sexp.to_string
    in
    QCheck.make ~print Gen.list_pair
end

let test_unary ~name ~op_bv ~op_bool =
  QCheck.Test.make ~count:1000 ~name
    Arbitrary.list
    (test_op1 ~op_bv ~op_bool)
  |> QCheck.Test.check_exn

let test_binary ~name ~op_bv ~op_bool =
  QCheck.Test.make ~count:1000 ~name
    Arbitrary.list_pair
    (test_op2 ~op_bv ~op_bool)
  |> QCheck.Test.check_exn

let%test_unit "not" =
  test_unary ~name:"not" ~op_bv:Fast_bitvector.Allocate.not ~op_bool:not

let%test_unit "and" =
  test_binary ~name:"and" ~op_bv:Fast_bitvector.Allocate.and_ ~op_bool:(&&)

let%test_unit "or" =
  test_binary ~name:"or" ~op_bv:Fast_bitvector.Allocate.or_ ~op_bool:(||)

let%test_unit "xor" =
  test_binary ~name:"xor" ~op_bv:Fast_bitvector.Allocate.xor ~op_bool:Bool.(<>)

let%test_unit "sexp round-trip (B0F)" =
  test_unary ~name:"sexp round-trip (B0F)" ~op_bv:(fun bv ->
      Fast_bitvector.Bit_zero_first.(t_of_sexp (sexp_of_t bv))) ~op_bool:Fn.id

let%test_unit "sexp round-trip (B0L)" =
  test_unary ~name:"sexp round-trip (B0L)" ~op_bv:(fun bv ->
      Fast_bitvector.Bit_zero_last.(t_of_sexp (sexp_of_t bv))) ~op_bool:Fn.id

let%test_unit "append" =
  QCheck.Test.make ~count:1000 ~name:"append"
    (QCheck.tup2 Arbitrary.list Arbitrary.list)
    (fun (l1, l2) ->
       let bv1 = Fast_bitvector.Bit_zero_first.of_bool_list l1 in
       let bv2 = Fast_bitvector.Bit_zero_first.of_bool_list l2 in
       let bv3 = Fast_bitvector.append bv1 bv2 in
       let l3 = Fast_bitvector.Bit_zero_first.to_bool_list bv3 in
       [%equal: bool list] l3 (l1 @ l2)
    )
  |> QCheck.Test.check_exn


let test_fold ~name ~folder ~rev =
  QCheck.Test.make ~count:1000 ~name
    Arbitrary.list
    (fun l ->
       let bv = Fast_bitvector.Bit_zero_first.of_bool_list l in
       let set =
         List.foldi l
           ~init:Int.Set.empty
           ~f:(fun i s b ->
               if b then Set.add s i else s)
       in
       let bv_fold =
         folder
           bv
           ~init:[]
           ~f:(fun acc i ->
               i :: acc)
       in
       let set_fold =
         Set.fold
           set
           ~init:[]
           ~f:(fun acc i ->
               i :: acc)
       in
       let bv_fold = List.rev bv_fold in
       let set_fold = if rev then set_fold else List.rev set_fold in
       if [%equal: int list] bv_fold set_fold
       then true
       else QCheck.Test.fail_reportf
           !"bv_fold: %{sexp:int list} != set_fold: %{sexp:int list}"
           bv_fold
           set_fold
    )
  |> QCheck.Test.check_exn

let%test_unit "fold_set" =
  test_fold ~name:"fold_set" ~folder:Fast_bitvector.fold_set ~rev:false

let%test_unit "fold_right_set" =
  test_fold ~name:"fold_right_set" ~folder:
    (fun bv ~init ~f ->
       Fast_bitvector.fold_right_set
         bv ~init ~f:(fun a b -> f b a)
    ) ~rev:true
