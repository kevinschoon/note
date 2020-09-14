#!/bin/bash
set -e

VERSION="$(git describe --always)"

scripts/generate_build_artifact.sh docker/Dockerfile.debian glibc
scripts/generate_deb_artifact.sh "$VERSION"

scripts/generate_build_artifact.sh docker/Dockerfile.alpine muslc

scripts/generate_arch_artifact.sh "$VERSION"
