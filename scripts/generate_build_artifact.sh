#!/bin/bash
# Generate a compiled build artifact. This is a hack because
# to the best of my knowledge the distribution story for OCaml 
# binaries is terrible AFAICT. Dynamic linking means we need to
# compile against the oldest version of GLIBC we wish to support.
# We also link against Alpine muslc for convenience. It is not
# possible to compile Mach-O Darwin executables from Linux, again,
# AFAICT. ARM binaries would be nice too. TODO: Use a Docker buildx
# workflow or perferably something without Docker at all.
set -e

DOCKER_FILE="$1"
PREFIX="$2"

_usage() {
 echo "USAGE: "
 echo "generate_build_artifact.sh DOCKERFILE PREFIX"
}

[[ -z "$DOCKER_FILE" ]] || [[ -z "$PREFIX" ]] && {
 _usage
 exit 1
}

# TODO: Capture more build specifics
VERSION="$(git describe --always)"

dune clean
dune build

PKG="note-$PREFIX-$VERSION"
mkdir -p "pkg/$PKG"
cp -rLv _build/install/default/* "pkg/$PKG/"
rm -vf "pkg/$PKG/bin/note"
docker build -t "$PKG" -f "$DOCKER_FILE" .
container_id="$(docker create $PKG)"
docker cp "$container_id:/usr/bin/note" "pkg/$PKG/bin/note"
docker rm "$container_id" 1>/dev/null

tar -C "pkg/$PKG" -czvf "pkg/$PKG.tar.gz" .
md5sum "pkg/$PKG.tar.gz" > "pkg/$PKG.tar.gz.md5"
