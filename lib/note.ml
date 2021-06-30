open Core

module Frontmatter = struct
  type t = { path : string; description : string option; tags : string list }

  let empty = { path = ""; description = None; tags = [] }

  let of_json ?(path = None) json =
    let path =
      match path with
      | Some path -> path
      | None -> (
          match Ezjsonm.find_opt json [ "path" ] with
          | Some path -> Ezjsonm.get_string path
          | None -> "")
    in
    let description =
      match Ezjsonm.find_opt json [ "description" ] with
      | Some description -> Some (Ezjsonm.get_string description)
      | None -> None
    in
    let tags =
      match Ezjsonm.find_opt json [ "tags" ] with
      | Some tags -> Ezjsonm.get_strings tags
      | None -> []
    in
    { path; description; tags }

  let to_json frontmatter =
    let content =
      [
        ("path", Ezjsonm.string frontmatter.path);
        ("tags", Ezjsonm.strings frontmatter.tags);
      ]
    in
    let content =
      match frontmatter.description with
      | Some value -> ("description", Ezjsonm.string value) :: content
      | None -> content
    in
    content |> Ezjsonm.dict
end

type note = { frontmatter : Frontmatter.t; content : string }

and tree = Tree of (note * tree list)

let root_template =
  {|
---
path: /
description: all notes decend from here
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

let of_string ?(path = None) content =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" content
  in
  if List.length indexes >= 2 then
    (* parsing the top half of the note *)
    let meta_str =
      String.slice content (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let frontmatter =
      meta_str |> Yaml.of_string_exn |> Frontmatter.of_json ~path
    in
    { frontmatter; content }
  else { frontmatter = Frontmatter.empty; content }

let root = Tree (of_string root_template, [])

let rec resolve_manifest ~path manifest =
  let items =
    match manifest |> Manifest.list ~path with
    | [] -> []
    | items ->
        items
        |> List.map ~f:(fun item ->
               let path = item.path in
               let slug = item.slug |> Slug.to_string in
               let note =
                 In_channel.read_all slug |> of_string ~path:(Some path)
               in
               Tree (note, manifest |> resolve_manifest ~path))
  in
  items

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
