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
