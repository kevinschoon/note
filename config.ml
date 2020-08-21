open Core

type t = { state_dir : string; lock_file : string; editor : string option }

let default_path =
  Filename.concat (Sys.home_directory ()) ".config/note/config.yaml"

let default_config =
  let home_dir = Sys.home_directory () in
  {
    state_dir = Filename.concat home_dir ".local/share/note";
    lock_file = Filename.concat home_dir ".local/share/note.lock";
    editor = None;
  }

let to_string config =
  let editor = match config.editor with Some name -> name | None -> "" in
  let dict =
    Ezjsonm.dict
      [
        ("state_dir", Ezjsonm.string config.state_dir);
        ("lock_file", Ezjsonm.string config.lock_file);
        ("editor", Ezjsonm.string editor);
      ]
  in
  Yaml.to_string_exn dict

let of_string config_str =
  let value = Yaml.of_string_exn config_str in
  let state_dir = Ezjsonm.get_string (Ezjsonm.find value [ "state_dir" ]) in
  let lock_file = Ezjsonm.get_string (Ezjsonm.find value [ "lock_file" ]) in
  let editor =
    if Ezjsonm.mem value [ "editor" ] then
      Some (Ezjsonm.get_string (Ezjsonm.find value [ "editor" ]))
    else None
  in
  { state_dir; lock_file; editor }

let get config key =
  match key with
  | "state_dir" -> Some config.state_dir
  | "lock_file" -> Some config.lock_file
  | _ -> None

let get_exn config key =
  let result = get config key in
  match result with
  | Some value -> value
  | None -> failwith (sprintf "bad configuration key: %s" key)

let initialize path config =
  (* ensure the directory exists *)
  ( match Sys.file_exists (Filename.dirname path) with
  | `Yes -> ()
  | `No | `Unknown -> () );
  (* write config if that file does not exist *)
  (let config_dir = Filename.concat (Sys.home_directory ()) ".config/note" in
   match Sys.file_exists config_dir with
   | `Yes -> ()
   | `No | `Unknown -> Unix.mkdir_p config_dir);
  (* write the config to disk if it does not already exist *)
  ( match Sys.file_exists path with
  | `Yes -> ()
  | `No | `Unknown ->
      let str_config = to_string config in
      Out_channel.write_all ~data:str_config path );
  (* create the state directory if it is missing *)
  ( match Sys.file_exists config.state_dir with
  | `Yes -> ()
  | `No | `Unknown -> Unix.mkdir_p config.state_dir );
  ()

let resolve config =
    let editor = match config.editor with
    | Some name -> Some name
    | None -> (Sys.getenv "NOTE_EDITOR") in
    { state_dir = config.state_dir ; lock_file = config.lock_file; editor = editor }

let read_config path =
  match Sys.file_exists path with
  | `Yes ->
      let config_str = In_channel.read_all path in
      (resolve (of_string config_str))
  | `No | `Unknown -> (resolve default_config)
