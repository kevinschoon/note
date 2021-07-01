module Frontmatter : sig
  type t = { path : string; description : string option; tags : string list }
end

type t

type tree = Tree of (t * tree list)

val fst : tree -> t

type options = {
  state_dir : string;
  editor : string;
  on_modification : string option;
}

val to_string : t -> string

val of_string : ?path:string option -> string -> t

val to_json : t -> Ezjsonm.value

val frontmatter : t -> Frontmatter.t
val content : t -> string

val flatten : ?accm:t list -> tree -> t list

val load : path:string -> options -> tree

val find : path:string -> options -> t option

val create :
  ?description:string option ->
  ?tags:string list ->
  ?content:string option ->
  path:string ->
  options ->
  unit

val remove : path:string -> options -> unit

val edit : path:string -> options -> unit
