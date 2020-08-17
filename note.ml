open Core
open Stdio

type task = { title : string; tags : string list }

let get_title dict =
  let title = List.find ~f:(fun (key, _) -> equal_string key "title") dict in
  match title with Some (_, v) -> Ezjsonm.get_string v | None -> ""

let get_tags dict =
  let title = List.find ~f:(fun (key, _) -> equal_string key "tags") dict in
  match title with Some (_, v) -> Ezjsonm.get_strings v | None -> []

let get_front_matter task_str =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" task_str
  in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice task_str
        (List.nth_exn indexes 0 + 3)
        (List.nth_exn indexes 1)
    in
    let value = Yaml.of_string_exn meta_str in
    let dict = Ezjsonm.get_dict value in
    Some { title = get_title dict; tags = get_tags dict }
  else None

let read_task path =
  let task_str = In_channel.read_all path in
  get_front_matter task_str

let read_tasks path =
  let files = Sys.ls_dir path in
  let paths = List.map ~f:(fun p -> sprintf "%s/%s" path p) files in
  List.map ~f:read_task paths

let display_task task =
  match task with
  | Some t ->
      print_endline t.title;
      print_endline (String.concat ~sep:" | " t.tags)
  | None -> print_endline "bad task :("
