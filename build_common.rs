mod common {
    use std::env;

    pub const MIN_SUPPORTED_VERSION: (u32, u32) = (2, 14);
    pub const MAX_SUPPORTED_VERSION: (u32, u32) = (2, 17);

    fn get_nif_version_from_env(version: &str) -> (u32, u32) {
        let parts: Vec<Result<_, _>> = version
            .split('.')
            .take(2)
            .map(|n| n.parse::<u32>())
            .collect();

        let mut nif_version = match &parts[..] {
            [Ok(major), Ok(minor)] => (*major, *minor),
            _other => panic!("The RUSTLER_NIF_VERSION is not a valid version"),
        };

        if nif_version < MIN_SUPPORTED_VERSION {
            panic!(
                "The NIF version given from RUSTLER_NIF_VERSION is not supported: {}.{}",
                nif_version.0, nif_version.1
            );
        }

        // TODO: This code will need adjustment if the Erlang developers ever decide to introduce
        //       a new major NIF version.
        if nif_version > MAX_SUPPORTED_VERSION {
            eprintln!(
                "NIF version {}.{} is not yet supported, falling back to {}.{}",
                nif_version.0, nif_version.1, MAX_SUPPORTED_VERSION.0, MAX_SUPPORTED_VERSION.1
            );
            nif_version = MAX_SUPPORTED_VERSION;
        }

        nif_version
    }

    pub fn handle_nif_version_from_env() -> Option<(u32, u32)> {
        println!("cargo:rerun-if-env-changed=RUSTLER_NIF_VERSION");
        env::var("RUSTLER_NIF_VERSION").ok().map(|val| {
            let nif_version = get_nif_version_from_env(&val);

            // Activate all config flags for the supported NIF versions
            for minor in 0..=nif_version.1 {
                println!(
                    "cargo:rustc-cfg=feature=\"nif_version_{}_{}\"",
                    nif_version.0, minor
                );
            }

            nif_version
        })
    }
}
