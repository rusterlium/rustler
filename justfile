build:
    cargo build
    cd rustler_mix && mix deps.get && mix compile

test:
    cargo test -q
    cd rustler_mix && mix deps.get && mix test
    cd rustler_tests && mix deps.get && mix test

test-all: test
    cd rustler_mix && ./test.sh

check-format:
    cargo fmt --all -- --check
    mix format --check-formatted

format:
    cargo fmt --all
    mix format

lint:
    cargo clippy --all-targets --all-features
    cd rustler_mix && mix credo --strict

regenerate-rustdoc:
    RUSTC_BOOTSTRAP=1 \
    RUSTDOC_OPTIONS="-Z unstable-options --output-format=json" \
    cargo doc --no-deps --all-features

    rustdoc-md \
        --path target/doc/rustler.json \
        --output doc/rust-api.md \

    rustdoc-md \
        --path target/doc/rustler_codegen.json \
        --output doc/rust-codegen.md \
