#!/bin/bash
set -e

docker build -t note-alpine -f docker/Dockerfile.alpine .
docker build -t note-debian -f docker/Dockerfile.debian .
