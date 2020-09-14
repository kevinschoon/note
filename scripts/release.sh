#!/bin/bash
set -e

VERSION="$(git describe --always)"

dune clean
dune build

scripts/generate_build.sh docker/Dockerfile.debian glibc
scripts/generate_deb.sh "$VERSION"

scripts/generate_build.sh docker/Dockerfile.alpine muslc

scripts/generate_pkgbuild.sh "$VERSION"
