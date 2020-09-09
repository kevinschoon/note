# Note 🐪

`Note` is a simple CLI based note taking application. 

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

Notes are stored in a format that is compatible with the static website generator [Hugo](https://gohugo.io/content-management/front-matter/) and thus you can point your `state_dir` to a Hugo content directory and get a web based interface for all of your notes.

## Subcommands

### Cat

Write one or more notes to stdout. By default the cat command will write every note to stdout as plain text however the encoding can be adjusted to `yaml` or `json` for consumption by other tools.

#### Examples

```bash
# print the parsed content of the fuubar note
note cat fuubar
# write all commands as a json list
note cat -encoding json
```

### Config

Display the current configuration as inferred by Note. It is also possible to extract specific values by specifying a key value.

#### Examples

```bash
# display the current configuration
note config
# extract a specific value from the configuration
note config -get state_dir
```

### Create

Create a new note and save it to disk in your configured `state_dir`. The `on_modification` call back will be invoked if the file is committed to disk. 

#### Examples

```bash
# create a new note with the given title and tags
note create "Remember The Milk" groceries fuu bar
# create a note by reading from stdin
note create -stdin <<EOF
# My Important Note

Hello World!
EOF
# the title will be inferred from the heading
note ls "My Important Note"
```

### Delete

Delete the first note that matches the filter criteria. The `on_modification` call back will be invoked if the note is deleted. 

#### Examples

```bash
# delete the note called fuubar

note delete fuubar
```

### Edit

Select a note that matches the filter criteria and open it in your `$EDITOR`. The `on_modification` call back will be invoked if the edited file differs from the original. 

#### Examples

```bash
# edit the fuubar note
note edit fuubar
```

### List
List notes that match the filter criteria, if no filter criteria is given all notes will be listed

#### Examples

```bash
# list all notes
note ls
```
