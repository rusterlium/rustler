#!/bin/bash
set -e

VERSION=$1

# Version format check
if ! [[ $VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Invalid version"
    exit -1
fi

# Check version unpublished
#CRATES_RET=`curl "https://crates.io/api/v1/crates/rustler/$VERSION/dependencies"`
#if ! [[ $CRATES_RET =~ "does not have a version" ]]; then
#    echo "Version already published"
#    exit -1
#fi

# Update versions in manifests
sed -i "s/^version = \"[^\"]*\" # rustler version$/version = \"$VERSION\" # rustler version/" rustler/Cargo.toml
sed -i "s/^version = \"[^\"]*\" # rustler_codegen version$/version = \"$VERSION\" # rustler_codegen version/" rustler_codegen/Cargo.toml
sed -i "s/def rustler_version, do: \"[^\"]*\"$/def rustler_version, do: \"$VERSION\"/" rustler_mix/mix.exs rustler_mix/lib/rustler.ex

# Verify that everything is OK by packaging/compiling
pushd rustler
cargo package --allow-dirty
popd
pushd rustler_codegen
cargo package --allow-dirty
popd
pushd rustler_mix
mix compile
popd
git status

echo
echo "This script will run:"
echo "                $ git commit -m \"(release) $VERSION\""
echo "rustler_mix     $ mix hex.publish"
echo "rustler         $ cargo publish"
echo "rustler_codegen $ cargo publish"
echo "                $ git push"
echo

read -p "Everything OK? [yN] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then

    # Commit
    git commit -m "(release) $VERSION"

    # Update and publish
    pushd rustler_mix
    mix hex.publish
    popd
    pushd rustler
    cargo publish
    popd
    pushd rustler_codegen
    cargo publish
    popd

    git push

fi
