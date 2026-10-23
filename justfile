default:
    @just --choose

setup:
    cd rustler_mix && mix deps.get
    cd rustler_tests && mix deps.get
    cargo fetch

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

regenerate:
    rustler/otp_headers/update.sh

doc:
    cd rustler_mix && mix docs

regenerate-rustdoc:
    cd rustler_mix && mix run -e 'Rustler.ExDoc.Retriever.generate_json!()'
