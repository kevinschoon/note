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
mkdir -p "$PKG_PATH/usr/share/bash-completion/completions"
mv "$PKG_PATH/share/note/note.bash" "$PKG_PATH/usr/share/bash-completion/completions/"
dpkg-deb -b "$PKG_PATH"
