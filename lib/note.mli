type t

val build : ?tags:string list -> ?content:string -> string -> t
(** build a new note *)

val get_title : t -> string
(** access the title of a note *)

val get_tags : t -> string list

val get_data : t -> Ezjsonm.value list
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

val to_json : t -> [>Ezjsonm.t]

val filter: ?keys: string list -> (t -> bool)
