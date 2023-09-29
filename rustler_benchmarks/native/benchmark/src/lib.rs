mod nif_record;
mod nif_struct;
mod nif_various;

rustler::init!(
    "Elixir.Benchmark",
    [
        nif_struct::benchmark,
        nif_record::benchmark,
        nif_various::encode_tagged_enum,
        nif_various::decode_tagged_enum,
        nif_various::decode_struct,
        nif_various::decode_struct_string,
        nif_various::decode_string,
        nif_various::decode_term,
        nif_various::void,
        nif_various::encode_atom,
        nif_various::compare_atom
    ]
);
