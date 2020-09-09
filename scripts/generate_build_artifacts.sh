#!/bin/bash
set -e

VERSION="$(git describe --always)"

# TODO: Capture more build specifics

[[ ! -d "pkg" ]] && mkdir pkg

dune clean

GLIBC_PKG="note-glibc-$VERSION"
MUSL_PKG="note-musl-$VERSION"

docker build -t "$GLIBC_PKG" -f docker/Dockerfile.debian .
# docker build -t "$MUSL_PKG" -f docker/Dockerfile.alpine .

glibc_container_id="$(docker create $GLIBC_PKG)"
#musl_container_id="$(docker create $MUSL_PKG)"

docker cp "$glibc_container_id:/usr/bin/note" "pkg/$GLIBC_PKG"
docker rm "$glibc_container_id" 1>/dev/null
chmod 755 "pkg/$GLIBC_PKG"
#docker cp "$musl_container_id:/usr/bin/note" "$MUSL_PKG"
#docker rm "$musl_container_id" 1>/dev/null
#chmod 755 "$MUSL_PKG"
