let bit_sequences ~total ~max_length =
  Seq.init total (fun _ ->
      Seq.init (Random.int max_length) (fun _ -> Random.bool ()))

let small_sequences = bit_sequences ~max_length:100
let medium_sequences = bit_sequences ~max_length:1000
let big_sequences = bit_sequences ~max_length:10000

let extend_inline_of_seq seqs =
  seqs |> Seq.map Fast_bitvector.of_bool_seq |> List.of_seq

let builder_of_seq seqs =
  seqs
  |> Seq.map Fast_bitvector.Builder.of_seq
  |> Seq.map Fast_bitvector.Builder.to_bitvector
  |> List.of_seq

let () =
  let total = 1000 in
  ignore
  @@ Benchmark.latency1 ~name:"extend_inline:small" 10000L extend_inline_of_seq
       (small_sequences ~total);
  ignore
  @@ Benchmark.latency1 ~name:"extend_inline:medium" 10000L extend_inline_of_seq
       (medium_sequences ~total);
  ignore
  @@ Benchmark.latency1 ~name:"extend_inline:big" 10000L extend_inline_of_seq
       (big_sequences ~total);
  ignore
  @@ Benchmark.latency1 ~name:"builder:small" 10000L builder_of_seq
       (small_sequences ~total);
  ignore
  @@ Benchmark.latency1 ~name:"builder:medium" 10000L builder_of_seq
       (medium_sequences ~total);
  ignore
  @@ Benchmark.latency1 ~name:"builder:big" 10000L builder_of_seq
       (big_sequences ~total)
