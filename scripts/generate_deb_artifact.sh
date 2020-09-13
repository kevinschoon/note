#!/bin/bash
# generate a deb package for debian based systems
set -e


_usage () {
    echo "USAGE: "
    echo "generate_debian_artifact.sh VERSION"
}

VERSION="$1"

[[ -z "$VERSION" ]] && {
    _usage
    exit 1
}

BUILD_ARTIFACT="pkg/note-glibc-$VERSION"
PKG="note-$VERSION-debian"
PKG_PATH="pkg/$PKG"
PKG_TARGET="$PKG_PATH.deb"
mkdir -p "$PKG_PATH/DEBIAN"

cat >"$PKG_PATH/DEBIAN/control"<<EOF
Package: note-ocaml
Version: $VERSION
Maintainer: Kevin Schoon <kevinschoon@gmail.com>
Architecture: amd64
Description: simple note taking cli
EOF

cp -r "$BUILD_ARTIFACT"/* "$PKG_PATH/"

# TODO: dune doesn't allow this path somehow
dpkg-deb --root-owner-group -b "$PKG_PATH"
md5sum "$PKG_TARGET" > "$PKG_TARGET.md5"
