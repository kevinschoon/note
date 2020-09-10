#!/bin/bash
set -e

scripts/generate_build_artifact.sh docker/Dockerfile.debian glibc
scripts/generate_deb_artifact.sh "$(git describe --always)"

scripts/generate_build_artifact.sh docker/Dockerfile.alpine muslc
