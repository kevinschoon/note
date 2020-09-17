note(1)

# NAME

note - a simple note taking command line interface

# SYNOPSIS

*note* COMMAND [OPTIONS...]

# DESCRIPTION

*note* is a command line interface for recording notes in the markdown format. It can be
configured to automatically commit modifications to a git repository and even synchronize
changes to a remote host.

# SUBCOMMANDS

## cat

output one or more notes to stdout

## config

display the configuration

## create

create a new note

## delete

delete an existing note

## edit

edit an existing note

## ls

list notes in the database

# EXAMPLES

## Create a new Note reading from stdin and save it to the state_dir
```

note create -stdin "Hello World" <<EOF

# An Important New Note 🐪

\```json
[1,2,3]
\```
EOF
```

# SEE ALSO

*note*(5)

# AUTHORS

Written by Kevin Schoon <kevinschoon@pm.me>. Source code available at https://github.com/kevinschoon/note
