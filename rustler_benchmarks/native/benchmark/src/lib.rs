mod nif_record;
mod nif_struct;

rustler::init!(
    "Elixir.Benchmark",
    [nif_struct::benchmark, nif_record::benchmark]
);
