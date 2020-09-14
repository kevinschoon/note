#!/bin/bash
# Generate a compiled build artifact. This is a hack because
# to the best of my knowledge the distribution story for OCaml 
# binaries is terrible. Dynamic linking means we need to
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

PKG="note-$PREFIX-$VERSION"
PKG_PATH="pkg/$PKG"
PKG_TARGET="pkg/$PKG.tar.gz"
SOURCE="$(realpath "_build/install/default")"

mkdir -p "$PKG_PATH"
pushd "$PKG_PATH"
mkdir -p usr/bin
mkdir -p usr/share/man/man1
cp "$SOURCE/man/man1/note.1" usr/share/man/man1/
mkdir -p usr/share/bash-completion/completions
cp "$SOURCE/share/note/note" usr/share/bash-completion/completions/
gzip usr/share/man/man1/note.1
popd

docker build -t "$PKG" -f "$DOCKER_FILE" .
container_id="$(docker create $PKG)"
docker cp "$container_id:/usr/bin/note" "$PKG_PATH/usr/bin/note"
docker rm "$container_id" 1>/dev/null
tar --owner root --group root -C "$PKG_PATH" -czvf "$PKG_TARGET" .
md5sum "$PKG_TARGET" > "$PKG_TARGET.md5"
