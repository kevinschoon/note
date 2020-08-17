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

Today will be a nice day.
```

## Configuration

The behavior of `note` can be configured with yaml file stored in `~/.config/note/config.yaml`.

```yaml
state_dir: ~/local/share/note/state
lock_file: ~/.local/share/note/note.lock
```

## State Dir

Notes are stored as flat files in the `state_dir` directory where each note is a flat file.
