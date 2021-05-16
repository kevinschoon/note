open Core

type operator = And | Or

and term = {
  titles : Re.Str.regexp list;
  tags : Re.Str.regexp list;
  operator : operator;
}

and note = {
  frontmatter : Ezjsonm.t;
  content : string;
  markdown : Omd.doc;
  slug : Slug.t;
  parent : term option;
}

let operator_of_string = function
  | "Or" -> Or
  | "And" -> And
  | _ -> failwith "invalid operator"

let term_of_json json =
  let titles =
    match Ezjsonm.find_opt json [ "titles" ] with
    | Some titles -> List.map ~f:Re.Str.regexp (Ezjsonm.get_strings titles)
    | None -> []
  and tags =
    match Ezjsonm.find_opt json [ "tags" ] with
    | Some tags -> List.map ~f:Re.Str.regexp (Ezjsonm.get_strings tags)
    | None -> []
  and operator =
    match Ezjsonm.find_opt json [ "operator" ] with
    | Some operator -> operator_of_string (Ezjsonm.get_string operator)
    | None -> Or
  in
  { titles; tags; operator }

let build ?(description = "") ?(tags = []) ?(content = "") ~title slug =
  let frontmatter =
    Ezjsonm.dict
      [
        ("title", Ezjsonm.string title);
        ("description", Ezjsonm.string description);
        ("tags", Ezjsonm.strings tags);
      ]
  in
  let markdown = Omd.of_string content in
  { frontmatter; content; markdown; slug; parent = None }

let get_title t =
  (* if title is specified use that, otherwise fall back to slug *)
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "title" ] with
  | Some title -> Ezjsonm.get_string title
  | None -> Slug.to_string t.slug

let get_description t =
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "description" ] with
  | Some description -> Ezjsonm.get_string description
  | None -> ""

let get_tags t =
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "tags" ] with
  | Some tags -> Ezjsonm.get_strings tags
  | None -> []

let get_path t = Slug.get_path t.slug

let rec extract_structured_data (accm : Ezjsonm.value list) (doc : Omd.doc) :
    Ezjsonm.value list =
  match doc with
  | [] -> accm
  | hd :: tl -> (
      match hd.bl_desc with
      | Code_block (kind, content) -> (
          match kind with
          | "json" ->
              let accm = accm @ [ Ezjsonm.from_string content ] in
              extract_structured_data accm tl
          | "yaml" ->
              let accm = accm @ [ Ezjsonm.wrap (Yaml.of_string_exn content) ] in
              extract_structured_data accm tl
          | _ -> extract_structured_data accm tl)
      | _ -> extract_structured_data accm tl)

let get_data t =
  let data = extract_structured_data [] t.markdown in
  Ezjsonm.list (fun value -> value) data

let to_json t =
  Ezjsonm.dict
    [
      ("frontmatter", Ezjsonm.value t.frontmatter);
      ("content", Ezjsonm.string t.content);
      ("data", get_data t);
    ]

let to_string t =
  let yaml = Yaml.to_string_exn (Ezjsonm.value t.frontmatter) in
  "\n---\n" ^ yaml ^ "\n---\n" ^ t.content

let of_string ~content slug =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" content
  in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice content (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let frontmatter : Ezjsonm.t =
      match Yaml.of_string_exn meta_str with
      | `O v -> `O v
      | `A v -> `A v
      | _ ->
          failwith
            "frontmatter is a partial fragment, should be either a dictionary \
             or list"
    in
    let parent = Ezjsonm.find_opt (Ezjsonm.value frontmatter) [ "parent" ] in
    let parent =
      match parent with Some json -> Some (term_of_json json) | None -> None
    in
    let markdown : Omd.doc =
      Omd.of_string
        (String.slice content
           (List.nth_exn indexes 1 + 3)
           (String.length content))
    in
    { frontmatter; content; markdown; slug; parent }
  else
    let frontmatter = Ezjsonm.dict [] in
    let markdown = Omd.of_string content in
    { frontmatter; content; markdown; slug; parent = None }

module Util = struct
  let split_words str =
    List.filter_map
      ~f:(fun x ->
        match String.strip ~drop:(fun x -> Char.equal x ' ') x with
        | "" -> None
        | _ -> Some x)
      (String.split ~on:' ' str)

  let rec to_words (accm : string list) (doc : Omd.doc) : string list =
    let split_words inline =
      match inline with Omd.Text text -> String.split ~on:' ' text | _ -> []
    in
    match doc with
    | [] -> accm
    | hd :: tl -> (
        (* TODO: account for headings, lists, etc *)
        match hd.bl_desc with
        | Paragraph inline ->
            let accm = accm @ split_words inline.il_desc in
            to_words accm tl
        | _ -> to_words accm tl)
end

module Encoding = struct
  let to_string ~style t =
    match style with
    | `Raw -> In_channel.read_all (get_path t)
    | `Json -> Ezjsonm.to_string (to_json t)
    | `Yaml -> Yaml.to_string_exn (to_json t)
    | `Html -> Omd.to_html t.markdown
end

let find_many ~term notes =
  let open Re.Str in
  let n_titles, n_tags = (List.length term.titles, List.length term.tags) in
  if n_titles + n_tags = 0 then notes
  else
    List.filter
      ~f:(fun note ->
        let has_title =
          let result =
            List.count
              ~f:(fun expr -> string_match expr (get_title note) 0)
              term.titles
          in
          match term.operator with Or -> result > 0 | And -> result = n_titles
        and has_tags =
          let result =
            List.count
              ~f:(fun expr ->
                Option.is_some
                  (List.find
                     ~f:(fun tag -> string_match expr tag 0)
                     (get_tags note)))
              term.tags
          in
          match term.operator with Or -> result > 0 | And -> result = n_tags
        in
        match term.operator with
        | Or -> has_title || has_tags
        | And -> has_title && has_tags)
      notes

let find_one ~term notes =
  let results = find_many ~term notes in
  match List.length results with 0 -> None | _ -> Some (List.hd_exn results)

let find_one_exn ~term notes =
  let result = find_one ~term notes in
  match result with
  | Some result -> result
  | None -> failwith "not found"

(* TODO terrible performance but who cares? *)
let resolve tree notes =
  List.fold ~init:tree
    ~f:(fun accm note ->
      let slug_id = Slug.to_string note.slug in
      match note.parent with
      | Some term -> (
          match find_one ~term notes with
          | Some parent -> (
              let parent_slug_id = Slug.to_string parent.slug in
              let children =
                List.Assoc.find ~equal:String.equal accm parent_slug_id
              in
              match children with
              | Some children ->
                  List.Assoc.add ~equal:String.equal accm parent_slug_id
                    (List.append children [ slug_id ])
              | None ->
                  List.Assoc.add ~equal:String.equal accm parent_slug_id
                    [ slug_id ])
          | None -> failwith "cannot resolve parent")
      | None ->
          if List.Assoc.mem ~equal:String.equal accm slug_id then accm
          else List.Assoc.add ~equal:String.equal accm slug_id [])
    notes

type node = Node of (note option * note list)

let dump_tree notes =
  let tree = resolve [] notes in
  let tree = resolve tree notes in
  List.iter
    ~f:(fun (key, values) ->
      print_endline (key ^ sprintf " -> %d" (List.length values)))
    tree

open ANSITerminal

let paint_tag (styles : Config.StylePair.t list) text : string =
  match List.find ~f:(fun entry -> String.equal entry.pattern text) styles with
  | Some entry -> sprintf entry.styles "%s" text
  | None -> sprintf [ Foreground Default ] "%s" text

let to_cells ~columns ~styles notes =
  let header =
    List.map
      ~f:(fun column ->
        let text_value = Config.Column.to_string column in
        let text_length = String.length text_value in
        let text_value = sprintf [ Bold; Underlined ] "%s" text_value in
        (text_value, text_length, 1))
      columns
  in
  let note_cells =
    let default_padding = 1 in
    List.fold ~init:[]
      ~f:(fun accm note ->
        accm
        @ [
            List.map
              ~f:(fun column ->
                match column with
                | `Title ->
                    let text_value = get_title note in
                    (text_value, String.length text_value, default_padding)
                | `Description ->
                    let text_value = get_description note in
                    (text_value, String.length text_value, default_padding)
                | `Tags ->
                    let text_value = String.concat ~sep:"|" (get_tags note) in
                    let text_length = String.length text_value in
                    let tags = get_tags note in
                    let tags =
                      List.map ~f:(fun tag -> paint_tag styles tag) tags
                    in
                    let text_value = String.concat ~sep:"|" tags in
                    (text_value, text_length, default_padding)
                | `WordCount ->
                    let text_value =
                      Core.sprintf "%d"
                        (List.length (Util.to_words [] note.markdown))
                    in
                    (text_value, String.length text_value, default_padding)
                | `Slug ->
                    let text_value = Slug.to_string note.slug in
                    (text_value, String.length text_value, default_padding))
              columns;
          ])
      notes
  in
  [ header ] @ note_cells
