#!/bin/bash
set -e

VERSION="$1"

TARBALL="note-glibc-$VERSION.tar.gz"
REMOTE_URL="https://github.com/kevinschoon/note/releases/download/$VERSION/$TARBALL"
MD5SUM="$(cat "pkg/$TARBALL.md5" |cut -d ' ' -f 1)"

cat >pkg/PKGBUILD-$VERSION<<EOF
pkgname=note
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
	install -Dm644 usr/share/bash-completion/completion/note -t \${pkgdir}/usr/share/bash-completion/completion/note
	install -Dm644 usr/share/man/man1/note.1.gz -t \${pkgdir}/usr/share/man/man1/
}
EOF
