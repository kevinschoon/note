type t

val build : ?tags:string list -> ?content:string -> title:string -> Slug.t -> t
(** build a new note *)

val get_title : t -> string
(** access the title of a note *)

val get_description : t -> string
(** access the description of a note *)

val get_path : t -> string
(** access the absolute path of a note *)

val to_string : t -> string
(** convert a note into a string *)

val of_string : data:string -> Slug.t -> t
(** decode a note from a string *)

module Encoding : sig
  val to_string : style:[< `Raw | `Json | `Yaml ] -> t -> string
end

module Filter : sig
  type strategy = Keys | Fulltext

  val find_one : ?strategy:strategy -> args:string list -> t list -> t option

  val find_many : ?strategy:strategy -> args:string list -> t list -> t list
end

module Display : sig
  type cell = string * ANSITerminal.style list

  type row = cell list

  val to_stdout : columns: Config.Column.t list -> style:[< `Fixed | `Simple | `Wide ] -> t list -> unit
end
