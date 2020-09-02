type t

val build : ?tags:string list -> ?content:string -> string -> t
(** build a new note *)

val get_title : t -> string
(** access the title of a note *)

val to_string : t -> string
(** convert a note into a string *)

val of_string : string -> t option
(** decode a note from a string *)

val to_json : t -> [>Ezjsonm.t]

val of_string_exn : string -> t

val read_note : string -> t option
(** read a note from the path *)

val read_note_exn : string -> t
(** read a note from the path raising an exception on failure *)

val read_notes : string list -> t list
(** read all of the note paths raising an exception of any failure *)

val read_notes_with_paths : string list -> (t * string) list
(** read all of the note paths returning a tuple of note and it's associated path rasiing an exception on any failure *)

val filter : t list -> string list -> t list
(** return a subset of notes that match any of the filter criteria *)

val filter_with_paths : (t * string) list -> string list -> (t * string) list
(** return a subset of notes that match any of the filter criteria with associated paths *)
