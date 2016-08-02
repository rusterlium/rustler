#[cfg(feature = "with-syntex")]
mod inner {
    extern crate easy_plugin;
    extern crate synthax;

    use std::env;
    use std::path::Path;

    pub fn main() {
        let out_dir = env::var_os("OUT_DIR").unwrap();
        println!("{:?}", out_dir);

        let src = Path::new("src/lib.rs.in");
        let dst1 = Path::new(&out_dir).join("lib.rs.1");
        let dst = Path::new(&out_dir).join("lib.rs");

        easy_plugin::expand(&src, &dst1).unwrap();
        synthax::expand(&dst1, &dst).unwrap();
    }
}

#[cfg(not(feature = "with-syntex"))]
mod inner {
    pub fn main() {}
}

fn main() {
    inner::main();
}
