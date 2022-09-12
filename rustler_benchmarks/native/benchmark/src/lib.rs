mod nif_struct;

rustler::init!("Elixir.Benchmark", [nif_struct::benchmark]);
