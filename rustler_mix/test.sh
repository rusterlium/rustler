#!/bin/bash
#
# Test if rustler_mix can be used to create a working example from the template.
#
# Note: This creates a temporary directory for the mix project. The directory is only cleaned up
# if the test works. This is done in order to allow investigating the error.
#

set -e

rustler_mix=$(realpath $(dirname $0))
rustler=$(realpath $rustler_mix/../rustler)
tmp=$(mktemp --directory)

export MIX_ARCHIVES="$tmp/mix_archives/"

#
# Test Steps
#
# * Create a new Elixir project
# * Add rustler as a dependency
# * Initialize a first crate from the template
# * Use rustler from the repository instead of from crates.io
# * Check that it compiles
# * Check that the module template in the generated README works
# * Check that the NIF can be loaded and used
#

echo "Build and install archive"
echo
mix local.hex --force
MIX_ENV=prod mix archive.build -o "$tmp/rustler.ez"
mix archive.install --force "$tmp/rustler.ez"

echo
echo "Creating a new mix project and rustler template in $tmp"
echo
cd $tmp

mkdir archives

mix new test_rustler_mix
cd test_rustler_mix

cat >mix.exs <<EOF
defmodule TestRustlerMix.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_rustler_mix,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application, do: [ ]

  defp deps, do: [ {:rustler, path: "$rustler_mix", runtime: false} ]
end
EOF

mix rustler.new --module RustlerMixTest --name rustler_mix_test || exit 1

mix deps.get || exit 1
mix deps.compile || exit 1

sed -i "s|^rustler.*$|rustler = { path = \"$rustler\" }|" native/rustler_mix_test/Cargo.toml

mix compile || exit 1

# Delete everything except the templated module from the generated README

delete=1
cat native/rustler_mix_test/README.md | while read line; do
    case "$line" in
      '```elixir')
        delete=0
        ;;
      '```'*)
        delete=1
        ;;
      *)
        if [ "$delete" -eq 0 ]; then
          echo "$line"
        fi
    esac
done > lib/rustler_mix_test.ex

cat >test/rustler_mix_test_test.exs <<EOF
defmodule RustlerMixTestTest do
  use ExUnit.Case

  test "can use generated nif" do
      assert RustlerMixTest.add(1, 2) == 3
  end
end
EOF

mix test || exit 1

# See https://github.com/rusterlium/rustler/issues/516, we also need to verify that everything
# we need is part of a release.
mix release || exit 1
_build/dev/rel/test_rustler_mix/bin/test_rustler_mix eval 'RustlerMixTest.add(1, 2)' || exit 1

echo "Done; cleaning up"
rm -r $tmp
