open Core
open Stdio

type t = {
  title : string;
  content : string;
  tags : string list;
  created : Time.t;
}

let get_title dict =
  let title = List.find ~f:(fun (key, _) -> equal_string key "title") dict in
  match title with Some (_, v) -> Ezjsonm.get_string v | None -> ""

let get_tags dict =
  let title = List.find ~f:(fun (key, _) -> equal_string key "tags") dict in
  match title with Some (_, v) -> Ezjsonm.get_strings v | None -> []

(* TODO *)
let get_created dict = Time.parse "20010102" ~fmt:"%Y%m%d" ~zone:Time.Zone.utc

let get_content note_str =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" note_str
  in
  if List.length indexes >= 2 then
    String.slice note_str (List.nth_exn indexes 1 + 3) (String.length note_str)
  else ""

let of_string note_str =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" note_str
  in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice note_str
        (List.nth_exn indexes 0 + 3)
        (List.nth_exn indexes 1)
    in
    let value = Yaml.of_string_exn meta_str in
    let dict = Ezjsonm.get_dict value in
    Some
      {
        title = get_title dict;
        content = get_content note_str;
        tags = get_tags dict;
        created = get_created dict;
      }
  else None

let read_note path =
  let note_str = In_channel.read_all path in
  of_string note_str

let read_notes path =
  let files = Sys.ls_dir path in
  let paths = List.map ~f:(fun p -> sprintf "%s/%s" path p) files in
  List.filter_map ~f:read_note paths

(* TODO: some how core List.mem does not work?!?!?! *)
let filter_note_by_tags note tags =
  match
    List.find
      ~f:(fun tag -> List.count ~f:(fun x -> equal_string x tag) note.tags > 0)
      tags
  with
  | Some x -> true
  | None -> false

let read_notes_filtered path filters =
  let notes = read_notes path in
  if List.length filters > 0 then
    (* first look by name *)
    let by_name =
      List.find
        ~f:(fun n -> equal_string n.title (List.nth_exn filters 0))
        notes
    in
    match by_name with
    | Some note -> [ note ]
    | None ->
        (* now we filter by tags *)
        List.filter ~f:(fun note -> filter_note_by_tags note filters) notes
  else notes

let display_note_fancy note =
  let created = Time.to_string note.created in
  let tag_string = String.concat ~sep:"|" note.tags in
  let formatted = Printf.sprintf "(%s) %s [%s]" created note.title tag_string in
  print_endline formatted;
  print_endline note.content

let display_note note = print_endline note.title
