open Core

module Operator = struct
  type t = And | Or

  let of_string = function
    | "Or" -> Or
    | "And" -> And
    | _ -> failwith "invalid operator"

  let to_string = function Or -> "Or" | And -> "And"
end

module Term = struct
  (* TODO: almost identical to frontmatter structure *)
  type t = {
    title : string list;
    description : string list;
    tags : string list;
  }

  let empty = { title = []; description = []; tags = [] }

  let of_json json =
    let title =
      match Ezjsonm.find_opt json [ "title" ] with
      | Some title -> Ezjsonm.get_strings title
      | None -> []
    in
    let description =
      match Ezjsonm.find_opt json [ "description" ] with
      | Some description -> Ezjsonm.get_strings description
      | None -> []
    in
    let tags =
      match Ezjsonm.find_opt json [ "tags" ] with
      | Some tags -> Ezjsonm.get_strings tags
      | None -> []
    in
    { title; description; tags }

  let to_json term =
    Ezjsonm.dict
      [
        ("title", Ezjsonm.strings term.title);
        ("description", Ezjsonm.strings term.description);
        ("tags", Ezjsonm.strings term.tags);
      ]
end

module Frontmatter = struct
  type t = {
    title : string;
    description : string;
    tags : string list;
    parent : Term.t option;
  }

  let empty = { title = ""; description = ""; tags = []; parent = None }

  let of_json json =
    let title =
      match Ezjsonm.find_opt json [ "title" ] with
      | Some title -> Ezjsonm.get_string title
      | None -> ""
    in
    let description =
      match Ezjsonm.find_opt json [ "description" ] with
      | Some description -> Ezjsonm.get_string description
      | None -> ""
    in
    let tags =
      match Ezjsonm.find_opt json [ "tags" ] with
      | Some tags -> Ezjsonm.get_strings tags
      | None -> []
    in
    let parent =
      match Ezjsonm.find_opt json [ "parent" ] with
      | Some parent -> Some (Term.of_json parent)
      | None -> None
    in
    { title; description; tags; parent }

  let to_json frontmatter =
    Ezjsonm.dict
      [
        ("title", Ezjsonm.string frontmatter.title);
        ("description", Ezjsonm.string frontmatter.description);
        ("tags", Ezjsonm.strings frontmatter.tags);
      ]
end

type note = {
  frontmatter : Frontmatter.t;
  content : string;
  slug : Slug.t option;
}

and tree = Tree of (note * tree list)

let root_template =
  {|
---
title: root
description: all of my notes decend from here
tags: []
---

# This is a Note!

|}

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

let to_json note =
  let data =
    note.content |> Omd.of_string |> extract_structured_data []
    |> Ezjsonm.list (fun value -> value)
  in
  Ezjsonm.dict
    [
      ("frontmatter", Frontmatter.to_json note.frontmatter);
      ("content", Ezjsonm.string note.content);
      ("data", data);
    ]

let to_string note =
  let yaml = Yaml.to_string_exn (Frontmatter.to_json note.frontmatter) in
  "\n---\n" ^ yaml ^ "\n---\n" ^ note.content

let of_string ?(slug = None) content =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" content
  in
  if List.length indexes >= 2 then
    (* parsing the top half of the note *)
    let meta_str =
      String.slice content (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let frontmatter = meta_str |> Yaml.of_string_exn |> Frontmatter.of_json in
    { frontmatter; content; slug }
  else { frontmatter = Frontmatter.empty; content; slug }

let rec flatten ~accm tree =
  let (Tree (note, others)) = tree in
  List.fold ~init:(note :: accm) ~f:(fun accm note -> flatten ~accm note) others

let match_term ?(operator = Operator.Or) ~(term : Term.t) note =
  let open Re.Str in
  let titles =
    List.map
      ~f:(fun exp ->
        let note_title = note.frontmatter.title in
        string_match exp note_title 0)
      (List.map ~f:regexp term.title)
  in
  let tags =
    List.map
      ~f:(fun expr ->
        Option.is_some
          (List.find
             ~f:(fun tag -> string_match expr tag 0)
             note.frontmatter.tags))
      (List.map ~f:regexp term.tags)
  in
  let results = List.concat [ titles; tags ] in
  (* if there are no conditions consider it matched *)
  if List.length results = 0 then true
  else
    match operator with
    | And ->
        List.length (List.filter ~f:(fun v -> v) results) = List.length results
    | Or -> List.length (List.filter ~f:(fun v -> v) results) > 0

let rec find_many ?(operator = Operator.Or) ~(term : Term.t) ~notes tree =
  let (Tree (note, others)) = tree in
  let notes =
    if match_term ~operator ~term note then note :: notes else notes
  in
  List.fold ~init:notes
    ~f:(fun accm note -> find_many ~operator ~term ~notes:accm note)
    others

let find_one ?(operator = Operator.Or) ~(term : Term.t) tree =
  tree |> find_many ~operator ~term ~notes:[] |> List.hd

let find_one_exn ?(operator = Operator.Or) ~(term : Term.t) tree =
  tree |> find_many ~operator ~term ~notes:[] |> List.hd_exn

let rec length tree =
  let (Tree (_, others)) = tree in
  List.fold ~init:(List.length others)
    ~f:(fun accm tree -> accm + length tree)
    others

let rec insert ?(operator = Operator.Or) ?(term = None) ~tree other =
  let (Tree (note, others)) = tree in
  match term with
  | Some term ->
      if match_term ~operator ~term note then
        (Tree (note, other :: others), true)
      else
        let others =
          List.map
            ~f:(fun tree -> insert ~operator ~term:(Some term) ~tree other)
            others
        in
        let result =
          List.fold ~init:([], false)
            ~f:(fun accm result ->
              let others, updated = accm in
              if updated then (fst result :: others, true)
              else (fst result :: others, snd result))
            others
        in
        let others, updated = result in
        (Tree (note, others), updated)
  | None -> (Tree (note, other :: others), true)

let buf_insert ~root notes =
  let tree =
    List.fold ~init:(root, [])
      ~f:(fun accm note ->
        let tree, buf = accm in
        let term = note.frontmatter.parent in
        let tree, inserted = insert ~term ~tree (Tree (note, [])) in
        let buf = if inserted then buf else note :: buf in
        (tree, buf))
      notes
  in
  tree

let rec resolve ~root notes =
  let tree, buf = buf_insert ~root notes in
  match buf |> List.length with 0 -> tree | _ -> resolve ~root:tree buf

let load path =
  let notes =
    path |> Slug.load
    |> List.map ~f:(fun slug ->
           slug.path |> In_channel.read_all |> of_string ~slug:(Some slug))
  in
  (* check if a "root" note is defined *)
  match
    List.find
      ~f:(fun note ->
        note
        |> match_term
             ~term:{ title = [ "__root" ]; description = []; tags = [] })
      notes
  with
  | Some root -> notes |> resolve ~root:(Tree (root, []))
  | None -> notes |> resolve ~root:(Tree (of_string root_template, []))

(* fancy output *)

module Util = struct
  open ANSITerminal

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

  let paint_tag (styles : Config.StylePair.t list) text : string =
    match
      List.find ~f:(fun entry -> String.equal entry.pattern text) styles
    with
    | Some entry -> sprintf entry.styles "%s" text
    | None -> sprintf [ Foreground Default ] "%s" text

  let to_cells ~columns ~styles (notes : note list) =
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
                      let text_value = note.frontmatter.title in
                      (text_value, String.length text_value, default_padding)
                  | `Description ->
                      let text_value = note.frontmatter.description in
                      (text_value, String.length text_value, default_padding)
                  | `Slug ->
                      let text_value =
                        match note.slug with
                        | Some slug -> slug |> Slug.shortname
                        | None -> "??"
                      in
                      (text_value, String.length text_value, default_padding)
                  | `Tags ->
                      let text_value =
                        String.concat ~sep:"|" note.frontmatter.tags
                      in
                      let text_length = String.length text_value in
                      let tags = note.frontmatter.tags in
                      let tags =
                        List.map ~f:(fun tag -> paint_tag styles tag) tags
                      in
                      let text_value = String.concat ~sep:"|" tags in
                      (text_value, text_length, default_padding)
                  | `WordCount ->
                      let text_value =
                        Core.sprintf "%d"
                          (List.length
                             (to_words [] (note.content |> Omd.of_string)))
                      in
                      (text_value, String.length text_value, default_padding))
                columns;
            ])
        notes
    in
    [ header ] @ note_cells
end

module Encoding = struct
  let to_string ~style t =
    match style with
    | `Raw -> t.content
    | `Json -> Ezjsonm.to_string (to_json t)
    | `Yaml -> Yaml.to_string_exn (to_json t)
    | `Html -> t.content |> Omd.of_string |> Omd.to_html
end
