# Note 🐪

`Note` is a hierarchical based note taking application. 

## Installation

### Package Managers

A package for ArchLinux is available on the [AUR](https://aur.archlinux.org/packages/note-ocaml).

## The Anatomy of a Note

A note is a simple markdown document that contains zero or more instances of structured data encoded as YAML or JSON. Notes can optionally contain `front-matter` at the head of each file, which is YAML or JSON enclosed by a pair of `---`.

```markdown
---
title: This is a Note
tags: [ocaml, programming]
---

# Hello World!

Today will be a nice day.
```

## Configuration

The behavior of `note` can be configured with yaml file stored in `~/.config/note/config.yaml` and configure itself per the XDG Base Directory [specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). You can view the resolved configuration by running `note config`:

```yaml
state_dir: /home/kevin/.local/share/note
lock_file: /home/kevin/.local/share/note.lock
editor: nvim
on_modification: note_commit.sh
```

## Structured Data

Notes that contain code blocks with structured data as well as front-matter are automatically parsed and exposed via the command API.

### Example

````bash
note create -stdin <<EOF

# Musical Styles

```yaml
- style: Bomba
  influenced:
  - style: Plena
  - style: Reggaetón
    influenced:
    - style: Bachatón
```
EOF

# Now let's inspect the code block with jq
note cat -encoding json "Musical Styles" | jq .data[0]
[
  {
    "style": "Bomba",
    "influenced": [
      {
        "style": "Plena"
      },
      {
        "style": "Reggaetón",
        "influenced": [
          {
            "style": "Bachatón"
          }
        ]
      }
    ]
  }
]
````

## The State Directory

Each note is stored as a flat file inside the `state_dir` with a slug that corresponds to the 
date (YYYYMMDD) on which it was created. If multiple notes are created on one day, an index will
be appended to the file.

```bash
$ tree .
.
├── manifest.json
├── note-20211123-0.md
├── note-20211123-1.md
├── note-20211123-2.md
├── note-20211123-3.md
├── note-20211123-4.md
├── note-20211123-5.md
├── note-20211123-6.md
├── note-20211123-7.md
├── note-20211123-8.md
└── note-20211123-9.md

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

Notes are stored in a format that is compatible with the static website generator [Hugo](https://gohugo.io/content-management/front-matter/) and thus you can point your `state_dir` to a Hugo content directory and get a web based interface for all of your notes.
