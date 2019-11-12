#!/bin/bash
#
# Release a new rustler version.
#
# Usage: ./release.sh 1.2.3
#
# ## Environment Variables
#
# * DRYRUN: Package release, but do not publish
#
set -e

VERSION=$1

# Version format check
if ! [[ $VERSION =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "Invalid version"
    exit -1
fi

TAG="rustler-$VERSION"

# FIXME get git version to revert to

# Check version unpublished
#CRATES_RET=`curl "https://crates.io/api/v1/crates/rustler/$VERSION/dependencies"`
#if ! [[ $CRATES_RET =~ "does not have a version" ]]; then
#    echo "Version already published"
#    exit -1
#fi

if [ ! -z "$(git status --untracked-files=no --porcelain)" ]; then
    echo "Uncommitted changes present; aborting."
    exit 1
fi

REVISION=$(git rev-parse --verify HEAD)

echo "Bumping versions.."

# Update versions in manifests
sed -i "s/^version = \"[^\"]*\" # rustler version$/version = \"$VERSION\" # rustler version/" rustler/Cargo.toml
sed -i "s/^rustler_codegen.*$/rustler_codegen = { path = \"..\/rustler_codegen\", version = \"$VERSION\", optional = true}/" rustler/Cargo.toml
sed -i "s/^version = \"[^\"]*\" # rustler_codegen version$/version = \"$VERSION\" # rustler_codegen version/" rustler_codegen/Cargo.toml
sed -i "s/def rustler_version, do: \"[^\"]*\"$/def rustler_version, do: \"$VERSION\"/" rustler_mix/mix.exs rustler_mix/lib/rustler.ex
sed -i "s/{:rustler, \".*\"}/{:rustler, \"~> $VERSION\"}/" rustler_mix/README.md

echo "Committing version.."
git commit -m "(release) $VERSION" rustler/Cargo.toml rustler_codegen/Cargo.toml rustler_mix/mix.exs rustler_mix/lib/rustler.ex rustler_mix/README.md

echo "Tagging version.."
git tag "$TAG"

# Verify that everything is OK by packaging/compiling
pushd rustler
cargo package
popd
pushd rustler_codegen
cargo package
popd
pushd rustler_mix
mix compile
popd
git status

echo
echo "This script will run:"
echo "rustler_mix     $ mix hex.publish"
echo "rustler         $ cargo publish"
echo "rustler_codegen $ cargo publish"
echo "                $ git push"
echo

read -p "Everything OK? [yN] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]] && [[ -z $DRYRUN ]]; then
    exit 1

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
    git push origin "$TAG"

fi
