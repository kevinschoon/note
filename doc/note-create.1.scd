note-create(1)

# NAME

note-create - Create a New Note

# SYNOPSIS

note create [options] TITLE [TAG ...]

# DESCRIPTION

Create a new note and save it to disk in your configured state_dir. The 
on_modification callback will be invoked if the file is committed to disk.

# OPTIONS

*positional arguments*

	*TITLE*
		name of the new note
	*TAG*
		string based tags

*optional arguments*

	*-stdin*
		read content from stdin and copy it into the note body

# EXAMPLES
```
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
