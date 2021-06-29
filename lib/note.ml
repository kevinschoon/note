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

  let is_empty term =
    [ term.title; term.description; term.tags ]
    |> List.fold ~init:[] ~f:(fun accm items ->
           match items |> List.length with 0 -> accm | _ -> items)
    |> List.length = 0

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

let root = Tree (of_string "", [])

let rec flatten ~accm tree =
  let (Tree (note, others)) = tree in
  List.fold ~init:(note :: accm) ~f:(fun accm note -> flatten ~accm note) others

let to_list tree =
  let (Tree (_, others)) = tree in
  List.fold ~init:[]
    ~f:(fun accm tree ->
      let (Tree (note, _)) = tree in
      note :: accm)
    others

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

let match_tree ?(operator = Operator.Or) ~(term : Term.t) tree =
  let (Tree (note, _)) = tree in
  note |> match_term ~operator ~term

let rec find_many ?(operator = Operator.Or) ~(term : Term.t) ~notes tree =
  let (Tree (note, others)) = tree in
  let notes =
    if match_term ~operator ~term note then note :: notes else notes
  in
  List.fold ~init:notes
    ~f:(fun accm note -> find_many ~operator ~term ~notes:accm note)
    others

let rec find_many_tree ?(operator = Operator.Or) ~(term : Term.t) ~trees tree =
  let (Tree (_, others)) = tree in
  let trees =
    if match_tree ~operator ~term tree then tree :: trees else trees
  in
  List.fold ~init:trees
    ~f:(fun accm tree -> find_many_tree ~operator ~term ~trees:accm tree)
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

let of_list ~context notes =
  (* check if a "root" note is defined *)
  let tree =
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
  in
  if Term.is_empty context then tree
  else
    let root = find_many_tree ~term:context ~trees:[] tree |> List.hd_exn in
    root

let load ~context path =
  let notes =
    path |> Slug.load
    |> List.map ~f:(fun slug ->
           slug.path |> In_channel.read_all |> of_string ~slug:(Some slug))
  in
  of_list ~context notes

let rec resolve_manifest ~root ~path manifest : tree =
  let others =
    manifest |> Manifest.list ~path
    |> List.map ~f:(fun item ->
           let path = item.slug |> Slug.to_string in
           let note = In_channel.read_all path |> of_string in
           let root = Tree (note, []) in
           resolve_manifest ~root ~path manifest)
  in
  let (Tree (root, _)) = root in
  Tree (root, others)
(*
module Adapter (M : sig
  val db : Manifest.t
end) =
struct
  let read path =
    let result = M.db |> Manifest.find ~path in
    match result with
    | Some entry ->
        let note = entry.slug |> In_channel.read_all |> of_string in
        note
    | None -> failwith "not found"

  let save ~path note =
    let description = note.frontmatter.description in
    let tags = note.frontmatter.tags in
    M.db |> Manifest.update ~path ~description ~tags
end

let rec resolve_manifest ~tree ~path manifest =
  let items = manifest |> Manifest.list ~path in
  let items =
    items
    |> List.map ~f:(fun item ->
           let logical_path = item |> Manifest.to_path ~manifest in
           let slug = item.slug |> Slug.of_string in
           let note =
             slug |> Slug.to_string |> In_channel.read_all
             |> of_string ~slug:(Some slug)
           in
           manifest
           |> resolve_manifest ~tree:(Tree (note, [])) ~path:logical_path)
  in
  let (Tree (root, _)) = tree in
  Tree (root, items)
  *)
