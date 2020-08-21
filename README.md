# Note 🐪

`Note` is a simple CLI based note taking application. 

## Usage

`Note` is self-documenting, all of it's commands can be explored by running `note help ...`

## The Anatonmy of a Note

A note is simply a markdown file, that has a `front-matter` at the head of the document. Front-matter is some structured data enclosed in a pair of `---`, we currently support yaml (and thus json). Everything above the front-matter section will be discarded and everything below it is considered
the body of a given note.

```markdown
---
title: This is a Note
tags: [ocaml, programming]
---

# Hello World!

🐪 Today will be a nice day 🐪.
```

## Configuration

The behavior of `note` can be configured with yaml file stored in `~/.config/note/config.yaml` and configure itself per the XDG Base Directory [specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). You can view the resolved configuration by running `note config`:

```yaml
state_dir: /home/kevin/.local/share/note
lock_file: /home/kevin/.local/share/note.lock
# optional items:
editor: nvim
on_modification: note_commit.sh
```

## The State Directory

Each note is stored as a flat file inside the `state_dir` with a slug that corresponds to the 
date (YYYYMMDD) on which it was created. If multiple notes are created on one day, an index will
be appended to the file.

```bash
$ tree ~/.local/share/note/
/home/kevin/.local/share/note/
├── note-20200818-1.md
├── note-20200819-1.md
├── note-20200819-2.md
└── note-20200819-3.md
```

## Persisting Notes with Git

If the `on_modification` configuration option is set it will be called each time a note is modified. This feature can be used to automatically commit your notes to a git repository with a script such as below:

```bash
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

## Web UI

Notes are stored in a format that is compatible with the static website generator [Hugo](https://gohugo.io/content-management/front-matter/) and thus you can point your `state_dir` to a Hugo content directory and get a web based interface for all of your notes (if you like this sort of thing).

## TODO

* [ ] Support alternate S-expression front-matter / search
* [ ] Add support for arbitrary meta-data filtering (currently it is limited to just title/tags)
* [ ] CI/CD With Github Actions
* [ ] Packaging (Go style ELF binaries)
* [ ] Package for AUR
