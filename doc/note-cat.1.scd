note-cat(1)

# NAME

note-cat - write notes to stdout

# SYNOPSIS

note cat [options] [FILTER ...]

# DESCRIPTION

Write one or more notes to stdout. By default the cat command will write every 
note to stdout as plain text however the encoding can be adjusted to yaml or 
json for consumption by other tools.

# OPTIONS

*positional arguments*

	*FILTER*
		tag or text based search terms

*optional arguments*

	*-encoding* 
		specify the output encoding

# EXAMPLES
```
# print the parsed content of the fuubar note
note cat fuubar
# write all commands as a json list
note cat -encoding json
```
