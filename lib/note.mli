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

  type strategy = Keys | Path | Subset

  val title : string -> t -> bool

  val tags : string -> t -> bool

  val subset : Ezjsonm.value -> t -> bool

  val jsonpath : ?other:Ezjsonm.value option -> Jsonpath.t -> t -> bool

  val of_strings : strategy -> string list -> (t -> bool) list

  val find_one : (t -> bool) list -> t list -> t option

  val find_one_with_paths : (t -> bool) list -> (t * string) list -> (t * string) option

  val find_many : (t -> bool) list -> t list -> t list
end
