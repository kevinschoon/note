type t
(** 

A slug is a human readable naming convention for storing note files in a directory.

The resulting directory will look something like below:

/home/kevin/.local/share/note/
├── note-20200820-0.md
├── note-20200820-1.md
└── note-20200821-0.md

Each slug has a prefix, the current date, and a number incremented from zero
indicating the number of notes that were taken on that date. 

NOTE: Epoch time would be simpler and more accurate but less friendly for reading.
*)

val to_string : t -> string
(** convert a slug into a string *)

val of_string : string -> t
(** convert a string into a slug *)

val load : string -> t list
(** read slugs from the given path *)

val next : t list -> t
(** create a new slug *)
