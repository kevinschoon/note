note-ls(1)

# NAME

note-ls - list notes

# SYNOPSIS

note ls [options] [FILTER ...]

# DESCRIPTION

List one or more notes that match the filter criteria, if no filter criteria 
is provided then all notes will be listed.

# OPTIONS

*positional arguments*

	*FILTER*
		tag or text based search terms

*optional arguments*

	*-columns* 
		columns to show in the note list

	*-style* 
		list style [fixed | wide | simple]

# EXAMPLES
```
# list notes
note ls
```
