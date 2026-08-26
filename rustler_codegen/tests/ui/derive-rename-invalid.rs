use rustler_codegen::NifMap;

#[derive(NifMap)]
struct InvalidRename {
    #[rustler(rename)]
    value: i32,
}

fn main() {}
