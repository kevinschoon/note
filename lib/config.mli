open Base

type t
(** configuration for the note cli *)

val default_path : string
(** the default configuration path *)

val to_string : t -> string
(** convert the configuration into a string *)

val of_string : string -> t
(** read the configuration from a string *)

val to_json : t -> Ezjsonm.t

val read_config : string -> t
(** read the configuration from a filesystem path *)

val initialize : string -> t -> unit
(** initialize the host system with the configuration *)

val get : t -> string -> string option
(** returns a key-value string pair from the configuration *)

val get_exn : t -> string -> string
(** returns a key-value string pair from the configuration and throws an exception if it is missing *)
