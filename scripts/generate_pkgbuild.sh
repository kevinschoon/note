#!/bin/bash
set -e

VERSION="$1"

TARBALL="note-glibc-$VERSION.tar.gz"
REMOTE_URL="https://github.com/kevinschoon/note/releases/download/$VERSION/$TARBALL"
MD5SUM="$(cat "pkg/$TARBALL.md5" |cut -d ' ' -f 1)"

cat >pkg/PKGBUILD-$VERSION<<EOF
# Maintainer: Kevin Schoon <kevinschoon@gmail.com>
pkgname=note-bin
pkgver=$VERSION
pkgrel=1
pkgdesc="Simple Note Taking CLI"
url="https://github.com/kevinschoon/note"
arch=(x86_64)
license=('AGPL3')
source=("$TARBALL::$REMOTE_URL")
md5sums=($MD5SUM)

package() {
	install -Dm755 usr/bin/note -t "\${pkgdir}/usr/bin/"
	install -Dm644 usr/share/bash-completion/completions/note -t \${pkgdir}/usr/share/bash-completion/completions/
	install -Dm644 usr/share/man/man7/note.7.gz -t \${pkgdir}/usr/share/man/man7/
}
EOF

pushd pkg
makepkg -p "PKGBUILD-$VERSION" --printsrcinfo > "SRCINFO-$VERSION"
popd
