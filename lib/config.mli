open Base

module ListStyle : sig
  type t = Fixed | Wide | Simple

  val of_string : string -> t

  val to_string : t -> string
end

module Encoding : sig
  type t = Json | Yaml | Raw

  val of_string : string -> t

  val to_string : t -> string
end

module Key : sig
  type t =
    | StateDir
    | LockFile
    | Editor
    | OnModification
    | ListStyle
    | Encoding

  val of_string : string -> t

  val to_string : t -> string
end

type t
(** configuration for the note cli *)

type value
(** a configuration value *)

val to_string : t -> string
(** convert the configuration into a string *)

val load : t
(** load the configuration from disk *)

val value_as_string : value -> string
(** convert a value to string form *)

val get : t -> Key.t -> value
(** get a single value by key *)

val get_string : t -> Key.t -> string
(** get a single value as a string by key *)

val get_string_opt : t -> Key.t -> string option
(** get a string option by key *)

