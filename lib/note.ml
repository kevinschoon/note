open Core
open Stdio

type t = { title : string; tags : string list; content : string }

let build ~title ~tags ~content = { title; tags; content }

let get_title t = t.title

let get_content t = t.content

let get_tags t = t.tags

let to_string ~note =
  let dict =
    Ezjsonm.dict
      [
        ("title", Ezjsonm.string note.title); ("tags", Ezjsonm.strings note.tags);
      ]
  in
  let front_matter = Yaml.to_string_exn dict in
  String.concat ~sep:"\n" [ "---"; front_matter; "---"; note.content ]

let of_string ~data =
  let indexes = String.substr_index_all ~may_overlap:true ~pattern:"---" data in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice data (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let content =
      String.slice data (List.nth_exn indexes 1 + 3) (String.length data)
    in
    let value = Yaml.of_string_exn meta_str in
    let title = Ezjsonm.get_string (Ezjsonm.find value [ "title" ]) in
    let tags = Ezjsonm.get_strings (Ezjsonm.find value [ "tags" ]) in
    Some { title; content; tags }
  else None

let of_string_exn ~data =
  let note = of_string ~data in
  match note with Some note -> note | None -> failwith "bad note content"

let read_note ~path =
  let data = In_channel.read_all path in
  of_string ~data

let read_note_exn ~path =
  let note = read_note ~path in
  match note with Some note -> note | None -> failwith "failed to read note"

let read_notes ~paths = List.map ~f:(fun path -> read_note_exn ~path) paths

let read_notes_with_paths ~paths =
  List.map ~f:(fun path -> (read_note_exn ~path, path)) paths

let filter notes filters =
  if List.length filters = 0 then notes
    (* return everything if there are no filters *)
  else
    List.fold ~init:[]
      ~f:(fun accm note ->
        (* first look by name *)
        let matches =
          List.count ~f:(fun filter -> String.equal note.title filter) filters
        in
        if matches > 0 then note :: accm
        else
          (* then compare each tag with each filter *)
          let matches =
            List.count
              ~f:(fun filter -> List.mem ~equal:String.equal note.tags filter)
              filters
          in
          if matches > 0 then note :: accm else accm)
      notes

let filter_with_paths notes filters =
  if List.length filters = 0 then notes
    (* return everything if there are no filters *)
  else
    List.fold
      ~init:([] : (t * string) list)
      ~f:(fun accm (note, path) ->
        (* first look by name *)
        let matches =
          List.count ~f:(fun filter -> String.equal note.title filter) filters
        in
        if matches > 0 then (note, path) :: accm
        else
          (* then compare each tag with each filter *)
          let matches =
            List.count
              ~f:(fun filter -> List.mem ~equal:String.equal note.tags filter)
              filters
          in
          if matches > 0 then (note, path) :: accm else accm)
      notes
