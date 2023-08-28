#!/bin/bash
#
# Prepare releasing a new rustler version.
#
# Usage: ./prepare_release.sh 1.2.3
#
set -e

VERSION=$1

# Version format check
SEMVER_REGEX="^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)(\\-[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?(\\+[0-9A-Za-z-]+(\\.[0-9A-Za-z-]+)*)?$"

if ! [[ $VERSION =~ $SEMVER_REGEX ]]; then
    echo "Invalid version"
    exit -1
fi

TAG="rustler-$VERSION"

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
sed -i "s/^rustler.*$/rustler = {path = \"..\/rustler\", version = \"$VERSION\"}/" rustler_bigint/Cargo.toml
sed -i "s/def rustler_version, do: \"[^\"]*\"$/def rustler_version, do: \"$VERSION\"/" rustler_mix/mix.exs rustler_mix/lib/rustler.ex
sed -i "s/@version .*$/@version \"$VERSION\"/" rustler_mix/mix.exs
sed -i "s/{:rustler, \".*\"/{:rustler, \"~> $VERSION\"/" rustler_mix/README.md

echo "Committing version.."
git commit -m "(release) $VERSION" \
    rustler/Cargo.toml rustler_codegen/Cargo.toml rustler_mix/mix.exs rustler_mix/lib/rustler.ex rustler_mix/README.md rustler_bigint/Cargo.toml

echo "Tagging version.."
git tag "$TAG"

cleanup() {
    if [[ -z $DONTREVERT ]]; then
	echo "Reverting changes.."
	git tag --delete "$TAG"
	git reset --hard "$REVISION"
    fi
}

trap cleanup INT EXIT

# Verify that everything is OK by compiling

cargo build
pushd rustler_mix
mix deps.get
mix compile
popd

read -p "Everything OK? [yN] " -n 1 -r

trap "" EXIT

cat <<_

To release, run the following manually:

cd rustler_codegen; cargo publish; cd -
cd rustler; cargo publish; cd -
cd rustler_mix; mix hex.publish; cd -

git push
git push origin "$TAG"
_
