module Frontmatter : sig
  type t = { path : string; description : string option; tags : string list }
  (* metadata in the heading of each markdown file seperated by --- *)
end

type t
(* a note represented as a tuple of frontmatter and raw text content *)

val to_string : t -> string
(* return a note with frontmatter and content *)

val of_string : ?path:string option -> string -> t
(* parse a note with optional frontmatter data *)

val to_json : t -> Ezjsonm.value
(* get a note as json data with structured data extracted from it *)

val frontmatter : t -> Frontmatter.t
(* get decoded frontmatter structure *)

val content : t -> string
(* get the raw text content without frontmatter heading *)

module Tree : sig
  type tree = Tree of (t * tree list)
  (* notes stored in a b-tree like data structure *)

  val fst : tree -> t
  (* return the top level note of a given tree *)

  val flatten : ?accm:t list -> tree -> t list
  (* flatten a tree into a list of notes *)
end

(* 
 *  high level adapter options for interaction from the CLI
 *)
type options = {
  state_dir : string;
  editor : string;
  on_modification : string option;
}
(* runtime options for interacting with the filesystem and manifest document*)

val load : path:string -> options -> Tree.tree
(* load all notes below the given path *)

val find : path:string -> options -> t option
(* find a single note *)

val create :
  ?description:string option ->
  ?tags:string list ->
  ?content:string option ->
  path:string ->
  options ->
  unit
(* create a new note opening it in an editor if no content is given *)

val remove : path:string -> options -> unit
(* remove an existing note *)

val edit : path:string -> options -> unit
(* edit an existing note opening it in the configured editor *)

(* helper functions for autocomplete *)
module Completion : sig
  val suggest_paths : hint:string -> options -> string list
  (* suggest paths for autocomplete *)

  val suggest_tags : hint:string -> options -> string list
  (* suggest tags for autocomplete *)
end
