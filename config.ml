open Core

type t = { state_dir : string; lock_file : string }

let config_path =
  Filename.concat (Sys.home_directory ()) ".config/note/config.yaml"

let get_string_key dict name =
  let title = List.find ~f:(fun (key, _) -> equal_string key name) dict in
  match title with Some (_, v) -> Ezjsonm.get_string v | None -> ""

let to_string config =
  let dict =
    Ezjsonm.dict
      [
        ("state_dir", Ezjsonm.string config.state_dir);
        ("lock_file", Ezjsonm.string config.lock_file);
      ]
  in
  Ezjsonm.to_string dict

let of_string config_str =
  (*
       TODO:  OCaml probably has something like Unmarshal
       or Serde to read serialized data into structures ?
  *)
  let value = Yaml.of_string_exn config_str in
  let dict = Ezjsonm.get_dict value in
  let state_dir = get_string_key dict "state_dir" in
  let lock_file = get_string_key dict "lock_file" in
  { state_dir; lock_file }

let default_config =
  let home_dir = Sys.home_directory () in
  {
    state_dir = Filename.concat home_dir ".local/share/note";
    lock_file = Filename.concat home_dir ".local/share/note.lock";
  }

let initialize config =
  (* ensure the directory exists *)
  ( match Sys.file_exists (Filename.dirname config_path) with
  | `Yes -> ()
  | `No | `Unknown -> () );
  (* write config if that file does not exist *)
  (let config_dir = Filename.concat (Sys.home_directory ()) ".config/note" in
   match Sys.file_exists config_dir with
   | `Yes -> ()
   | `No | `Unknown -> Unix.mkdir_p config_dir);
  (* write the config to disk if it does not already exist *)
  ( match Sys.file_exists config_path with
  | `Yes -> ()
  | `No | `Unknown ->
      let str_config = to_string config in
      Out_channel.write_all ~data:str_config config_path );
  (* create the state directory if it is missing *)
  ( match Sys.file_exists config.state_dir with
  | `Yes -> ()
  | `No | `Unknown -> Unix.mkdir_p config.state_dir );
  ()

let read =
  match Sys.file_exists config_path with
  | `Yes ->
      let config_str = In_channel.read_all config_path in
      of_string config_str
  | `No | `Unknown -> default_config
