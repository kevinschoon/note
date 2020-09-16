type t

val build : ?tags:string list -> ?content:string -> string -> t
(** build a new note *)

val get_title : t -> string
(** access the title of a note *)

val get_tags : t -> string list
(** access tags in the frontmatter of a note *)

val tokenize : t -> string list
(** split each word from the note into a list of string tokens *)

val get_data : t -> Ezjsonm.t
(** Extract arbitrarily nested data in the note's markdown document. 
    Currently this will only support code blocks of json or ymal but it may
    be expanded. For example a markdown document such as:

    # Title
    foo bar

    ## Code Examples

    ```json
    {"fuu": [{"bar": ["baz", "qux"]}]}
    ```
    ```yaml
    hello: world
    ```

    will return an Ezjsonm.value list in the order they are declared, e.g.

    [
        {"fuu": [{"bar": ["baz", "qux"]}]},
        {"hello": "world"},
    ]
 *)

val to_string : t -> string
(** convert a note into a string *)

val of_string : string -> t
(** decode a note from a string *)

val to_json : t -> [> Ezjsonm.t ]

module Filter : sig
  type strategy = Keys | Fulltext

  val find_one : ?strategy:strategy -> args:string list -> t list -> t option

  val find_one_with_paths :
    ?strategy:strategy ->
    args:string list ->
    (t * string) list ->
    (t * string) option

  val find_many : ?strategy:strategy -> args:string list -> t list -> t list
end

module Display : sig
  type style = Fancy | Simple

  type cell = string * ANSITerminal.style list

  type row = cell list

  val print_short : style:style -> t list -> unit
end
