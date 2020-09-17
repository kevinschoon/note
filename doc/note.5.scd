note(5)

# NAME

note - configuration file and format

## The Anatomy of a Note
A note is a simple markdown document that contains zero or more instances of structured data encoded as YAML or JSON. Notes can optionally contain `front-matter` at the head of each file, which is YAML or JSON enclosed by a pair of `---`.

```
---
title: This is a Note
tags: [ocaml, programming]
---

# Hello World!

Today will be a nice day.
```

## Configuration

The behavior of *note* can be configured with yaml file stored in *~/.config/note/config.yaml* and configure itself per the XDG Base Directory specification, see https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html. You can view the resolved configuration by running *note config*:

```
state_dir: /home/kevin/.local/share/note
lock_file: /home/kevin/.local/share/note.lock
editor: nvim
on_modification: note_commit.sh
```

## Persisting Notes with Git

If the `on_modification` configuration option is set it will be called each time a note is modified. This feature can be used to automatically commit your notes to a git repository with a script such as below:

```
#!/bin/bash
# This is an example script that can be used in conjunction with the
# on_modification config option to commit every note change to a git
# repository. You'll need to place it somewhere on your $PATH. This
# script assumes that your state_dir is also a git repository.
set -e

STATE_DIR="$(note config -get state_dir)"

pushd "$STATE_DIR"
git add --all
git commit -m 'automated commit'
popd
```
