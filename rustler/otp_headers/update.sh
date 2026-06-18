#!/bin/bash

set -eo pipefail

declare -rA versions=(
    ["2_14"]="21"
    ["2_15"]="22"
    ["2_16"]="24"
    ["2_17"]="26"
    ["2_18"]="29"
)

script_dir="$(dirname "$0")"

echo "Downloading OTP NIF header files..."

for nif_version in "${!versions[@]}"; do
    otp_version="${versions[$nif_version]}"
    otp_branch="maint-$otp_version"

    this_dir="$script_dir/nif_version_$nif_version/"
    mkdir -p "$this_dir" || true

    url="https://raw.githubusercontent.com/erlang/otp/refs/heads/${otp_branch}/erts/emulator/beam/erl_nif_api_funcs.h"
    header_file="$this_dir/erl_nif_api_funcs.h"

    echo "NIF $nif_version => OTP $otp_version..."

    echo "   Downloading header..."
    curl -sSf "$url" --output "$header_file"

    echo "   Processing header..."

    # Only use the section guarded by ERL_NIF_API_FUNC_DECL, we are not
    # interested in the `inline` functions or other parts of this header.
    section=$(sed -n '/#ifdef ERL_NIF_API_FUNC_DECL/,/#endif \/\* ERL_NIF_API_FUNC_DECL \*\//p' "$header_file")

    for sizeof_long in 4 8
    do
        echo "   Generating Rust API for SIZEOF_LONG=$sizeof_long..."
        output="$this_dir/api.$sizeof_long.rs"

        # Run the header through the C preprocessor and then our codegen script
        cpp -D "SIZEOF_LONG=$sizeof_long" \
            --include "$script_dir/preamble.h" \
            <<<"$section" \
            | cargo run --quiet --manifest-path "$script_dir/codegen/Cargo.toml" -- \
              --ulong-size="$sizeof_long" \
            > "$output"
    done
done
