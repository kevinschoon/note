note(1)

# NAME

*Note* is a simple CLI based note taking application

# SYNOPSIS

*note* COMMAND [OPTIONS...]

# DESCRIPTION

*note* is a command line interface for recording notes in the markdown format.
It maintains a directory of plain text files with a preamble of structured 
data for indexing, reference, and additional functionality.

# SYNCHRONIZING

Synchronization can be accomplished by writing shell scripts that are invoked
by the *note* program. Note is shipped with some example scripts that you can
find with your package manager.

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

note cat "Hello World" | jq '.data[0] | add'
14

# SEE ALSO

*note*(5)

# AUTHORS

Written by Kevin Schoon <kevinschoon@pm.me>. 
Source code available at https://github.com/kevinschoon/note
