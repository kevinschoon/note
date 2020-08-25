open Core

type t = {
  state_dir : string;
  lock_file : string;
  editor : string option;
  on_modification : string option;
}

let default_path =
  Filename.concat (Sys.home_directory ()) ".config/note/config.yaml"

let default_config =
  let home_dir = Sys.home_directory () in
  {
    state_dir = Filename.concat home_dir ".local/share/note";
    lock_file = Filename.concat home_dir ".local/share/note.lock";
    editor = None;
    on_modification = None;
  }

let to_string config =
  let editor =
    match config.editor with
    | Some value -> Ezjsonm.string value
    | None -> Ezjsonm.unit ()
  in
  let on_mod =
    match config.on_modification with
    | Some value -> Ezjsonm.string value
    | None -> Ezjsonm.unit ()
  in
  let dict =
    Ezjsonm.dict
      [
        ("state_dir", Ezjsonm.string config.state_dir);
        ("lock_file", Ezjsonm.string config.lock_file);
        ("editor", editor);
        ("on_modification", on_mod);
      ]
  in
  Yaml.to_string_exn dict

let of_string config_str =
  let value = Yaml.of_string_exn config_str in
  let state_dir = Ezjsonm.get_string (Ezjsonm.find value [ "state_dir" ]) in
  let lock_file = Ezjsonm.get_string (Ezjsonm.find value [ "lock_file" ]) in
  let string_or_none key =
    match Ezjsonm.find_opt value [ key ] with
    | Some v -> (
        match v with
        | `String v -> Some v
        | `Null -> None
        | _ ->
            failwith
              (sprintf "config entry %s must either be a string or NULL" key) )
    | None -> None
  in
  let editor = string_or_none "editor" in
  let on_modification = string_or_none "on_modification" in
  { state_dir; lock_file; editor; on_modification }

let get config key =
  match key with
  | "state_dir" -> Some config.state_dir
  | "lock_file" -> Some config.lock_file
  | "editor" -> (
      match config.editor with
      | Some v -> Some v
      | None -> (
          match Sys.getenv "EDITOR" with
          | Some v -> Some v
          | None ->
              failwith
                "No editor is specified in your configuration and environment \
                 variable $EDITOR is not set" ) )
  | "on_modification" -> config.on_modification
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
  let editor =
    match config.editor with
    | Some name -> Some name
    | None -> Sys.getenv "NOTE_EDITOR"
  in
  {
    editor;
    state_dir = config.state_dir;
    lock_file = config.lock_file;
    on_modification = config.on_modification;
  }

let read_config path =
  match Sys.file_exists path with
  | `Yes ->
      let config_str = In_channel.read_all path in
      resolve (of_string config_str)
  | `No | `Unknown -> resolve default_config
