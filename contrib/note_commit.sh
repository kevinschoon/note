#!/bin/bash
# This is an example script that can be used in conjunction with the
# on_modification config option to commit every note change to a git
# repository. You'll need to place it somewhere on your $PATH. This
# script assumes that your state_dir is also a git repository.
set -e

STATE_DIR="$(note config get state_dir)"

pushd "$STATE_DIR"
git add --all
git commit -m 'automated commit'
popd
