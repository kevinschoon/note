# Note

`Note` is a simple CLI based note taking application.

## The Anatonmy of a Note

A note is simply a text file, often written in Markdown, that has `front-matter` at the head of
the document.  Front-matter was popularized by static html generators such as Jekyll and Hugo. 
Everything above the front-matter section will be discarded and everything below it is considered
the body of a given note.

```markdown
---
title: This is a Note
tags: ["misc", "note taking"]
---

# Hello World!

🐪 Today will be a nice day 🐪.
```

## Configuration

The behavior of `note` can be configured with yaml file stored in `~/.config/note/config.yaml`.

```yaml
state_dir: ~/local/share/note/state
lock_file: ~/.local/share/note/note.lock
```

## State Dir

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
