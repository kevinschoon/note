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

type t = { frontmatter : Frontmatter.t; content : string }

let frontmatter note = note.frontmatter

let content note = note.content

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

let to_html note =
  note.content |> Omd.of_string |> Omd.to_html

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
    (* read second half of note as "content" *)
    let content = String.slice content (List.nth_exn indexes 1 + 3) 0 in
    { frontmatter; content }
  else { frontmatter = Frontmatter.empty; content }

module Tree = struct
  type tree = Tree of (t * tree list)

  let flatten tree =
    let rec flatten ~accm tree =
      let (Tree (note, others)) = tree in
      List.fold ~init:(note :: accm)
        ~f:(fun accm note -> flatten ~accm note)
        others
    in
    tree |> flatten ~accm:[]

  let fst tree =
    let (Tree (note, _)) = tree in
    note

  let note_to_json = to_json

  let rec to_json tree = 
    let (Tree (root, others)) = tree in
    Ezjsonm.dict [
      ("note", (root |> note_to_json)) ;
      ("descendants", others |> List.map ~f:to_json |> Ezjsonm.list (fun a -> a))
    ]

  let rec resolve_manifest ~path manifest =
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
end

(* high level adapter *)
module Adapter = struct
  type options = {
    state_dir : string;
    editor : string;
    on_modification : string option;
  }

  let editor_command ~editor path = Format.sprintf "%s %s" editor path

  let run_or_noop command =
    match command with Some command -> command |> Sys.command_exn | None -> ()

  let load ~path options =
    let manifest = options.state_dir |> Manifest.load_or_init in
    (* initialize the root note *)
    let root =
      match manifest |> Manifest.find ~path with
      | Some item ->
          item.slug |> Slug.to_string |> In_channel.read_all |> of_string
      | None -> (
          match path with
          | "/" ->
              let manifest = manifest |> Manifest.create ~path:"/" in
              let last = manifest.items |> List.hd_exn in
              let slug = last.slug |> Slug.to_string in
              let root = root_template |> of_string in
              slug |> Out_channel.write_all ~data:(root |> to_string);
              manifest |> Manifest.save;
              root
          | _ -> failwith "not found")
    in
    Tree.Tree (root, manifest |> Tree.resolve_manifest ~path)

  let find ~path options =
    let manifest = options.state_dir |> Manifest.load_or_init in
    let item = manifest |> Manifest.find ~path in
    match item with
    | Some item ->
        let slug = item.slug in
        let note = slug |> Slug.to_string |> In_channel.read_all |> of_string in
        Some note
    | None -> failwith "not found"

  let create ?(description = None) ?(tags = []) ?(content = None) ~path options
      =
    let manifest = options.state_dir |> Manifest.load_or_init in
    let manifest = manifest |> Manifest.create ~path in
    let item = manifest.items |> List.hd_exn in
    let path = item.path in
    let slug = item.slug in
    (match content with
    | Some content ->
        let note = { frontmatter = { path; description; tags }; content } in
        slug |> Slug.to_string |> Out_channel.write_all ~data:(note |> to_string)
    | None ->
        let note =
          { frontmatter = { path; description; tags }; content = "" }
        in
        slug |> Slug.to_string |> Out_channel.write_all ~data:(note |> to_string);
        slug |> Slug.to_string
        |> editor_command ~editor:options.editor
        |> Sys.command_exn);
    options.on_modification |> run_or_noop;
    manifest |> Manifest.save

  let remove ~path options =
    let manifest = options.state_dir |> Manifest.load_or_init in
    let item = manifest |> Manifest.find ~path in
    match item with
    | Some item ->
        let slug = item.slug in
        let manifest = manifest |> Manifest.remove ~path in
        slug |> Slug.to_string |> Unix.remove;
        options.on_modification |> run_or_noop;
        manifest |> Manifest.save
    | None -> failwith "not found"

  let edit ~path options =
    let manifest = options.state_dir |> Manifest.load_or_init in
    let item = manifest |> Manifest.find ~path in
    match item with
    | Some item ->
        let slug = item.slug in
        slug |> Slug.to_string
        |> editor_command ~editor:options.editor
        |> Sys.command_exn;
        let note = slug |> Slug.to_string |> In_channel.read_all |> of_string in
        let adjusted_path = note.frontmatter.path in
        (if not (Filename.equal adjusted_path item.path) then
         let manifest =
           manifest |> Manifest.move ~source:item.path ~dest:adjusted_path
         in
         manifest |> Manifest.save);
        options.on_modification |> run_or_noop
    | None -> failwith "not found"
end

include Adapter

module Completion = struct
  let suggest_paths ~hint options =
    options.state_dir |> Manifest.load_or_init
    |> Manifest.list ~path:(hint |> Filename.dirname)
    |> List.map ~f:(fun item -> item.path)
    |> List.filter ~f:(fun path -> path |> String.is_substring ~substring:hint)

  let suggest_tags ~hint options =
    let manifest = options.state_dir |> Manifest.load_or_init in
    manifest.items
    |> List.concat_map ~f:(fun item ->
           let frontmatter =
             item.slug |> Slug.to_string |> In_channel.read_all |> of_string
             |> frontmatter
           in
           frontmatter.tags)
    |> List.filter ~f:(fun tag -> tag |> String.is_substring ~substring:hint)
end
