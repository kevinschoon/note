(* TODO: somehow optional labels do not work the way I think they do *)

val create : callback:string option -> content:string -> string -> unit
(** write a new note to the destination and optionally run the callback *)

val create_on_change :
  callback:string option -> editor:string -> string -> string -> unit
(** write a new note to the destination and optionally run the callback, if the content is not modified, nothing will be written to the filesystem. *)

val edit : callback:string option -> editor:string -> string -> unit
(** modify an existing note and optionally read the callback on change *)

val delete : callback:string option -> title:string -> string -> unit
(** delete an existing note from the filesystem, optionally run callback on change *)
