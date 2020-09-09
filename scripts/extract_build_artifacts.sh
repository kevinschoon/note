#!/bin/bash
set -e

# TODO: Capture more build specifics

musl_container_id="$(docker create note-alpine)"
glibc_container_id="$(docker create note-debian)"

docker cp "$glibc_container_id:/usr/bin/note" note-glibc-latest
docker rm "$glibc_container_id" 1>/dev/null
chmod 755 note-glibc-latest
docker cp "$musl_container_id:/usr/bin/note" note-musl-latest
docker rm "$musl_container_id" 1>/dev/null
chmod 755 note-musl-latest
