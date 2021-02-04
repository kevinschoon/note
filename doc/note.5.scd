note(5)

# NAME

note - design and configuration

# The Anatomy of a Note
A note is a simple markdown document that contains zero or more instances of 
structured data encoded as YAML or JSON. Notes can optionally contain 
*front-matter* at the head of each file, which is YAML or JSON enclosed by a 
pair of "*---*".

```
---
title: This is a Note
description: ""
tags: [ocaml, programming]
---

# Hello World!

Today will be a nice day.

```

# The State Directory

Each note is stored as a flat file inside the *state_dir* with a slug that 
corresponds to the date (YYYYMMDD) on which it was created. If multiple notes 
are created on one day, an index will be appended to the file.

```
$ tree ~/.local/share/note/
/home/kevin/.local/share/note/
├── note-20200818-1.md
├── note-20200819-1.md
├── note-20200819-2.md
└── note-20200819-3.md
```

# Configuration

The behavior of *note* can be modified with yaml file stored in the XDG Base
Directory, e.g. *~/.config/note/config.yaml*. The configuration can be modified
with the *note config* subcommand.

## Options

*state_dir* directory to save notes in

*lock_file* lockfile to aquire for io operations

*editor* your text editor 

*on_modification* hook to run when a note is modified

*on_sync* hook to run when *note sync* is called

*list_style* style to output the list of notes with

*encoding* encoding for note output

*column_list* columns to include in note output

*styles* map to styles to apply to tags

## Example

```
lock_file: /home/kevin/.local/share/note.lock
editor: nvim
on_modification: note-on-modification
on_sync: note-on-sync
list_style: fixed
encoding: raw
column_list:
- title
- description
- tags
- words
styles:
- pattern: linux
  style:
  - Foreground White
  - Background Red
- pattern: ocaml
  style:
  - Foreground White
  - Background Green
  - Underlined

```

# Git Storage and Synchronization

## Persisting Notes with Git

If the *on_modification* configuration option is set it will be called each 
time a note is modified. This feature can be used to automatically commit your 
notes to a git repository. An example script is included with this package and
should be available on your path as *note-on-modification*.

## Synchronization with Git

The *on_sync* configuration option can be used to synchronize and resolve
conflicts with git backed storage. An example script is included with this 
package and should be available on your path as *note-on-sync*.

