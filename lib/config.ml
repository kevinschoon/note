open Core

let home = Sys.home_directory ()

let base_xdg_config_path = Filename.concat home ".config"

let base_xdg_share_path = Filename.concat home ".local/share"

let config_path =
  match Sys.getenv "NOTE_CONFIG" with
  | Some path -> path
  | None -> Filename.concat base_xdg_config_path "/note/config.yaml"

module ListStyle = struct
  type t = [ `Fixed | `Wide | `Simple ]

  let to_string = function
    | `Fixed -> "fixed"
    | `Wide -> "wide"
    | `Simple -> "simple"

  let of_string = function
    | "fixed" -> `Fixed
    | "wide" -> `Wide
    | "simple" -> `Simple
    | key -> failwith key
end

module Encoding = struct
  type t = [ `Json | `Yaml | `Raw ]

  let to_string = function `Json -> "json" | `Yaml -> "yaml" | `Raw -> "raw"

  let of_string = function
    | "json" -> `Json
    | "yaml" -> `Yaml
    | "raw" -> `Raw
    | key -> failwith (sprintf "unsupported encoding type: %s" key)
end

module Key = struct
  type t =
    [ `StateDir
    | `LockFile
    | `Editor
    | `OnModification
    | `ListStyle
    | `Encoding ]

  let all =
    [ `StateDir; `LockFile; `Editor; `OnModification; `ListStyle; `Encoding ]

  let of_string = function
    | "state_dir" -> `StateDir
    | "lock_file" -> `LockFile
    | "editor" -> `Editor
    | "on_modification" -> `OnModification
    | "list_style" -> `ListStyle
    | "encoding" -> `Encoding
    | key -> failwith (sprintf "bad configuration key %s" key)

  let to_string = function
    | `StateDir -> "state_dir"
    | `LockFile -> "lock_file"
    | `Editor -> "editor"
    | `OnModification -> "on_modification"
    | `ListStyle -> "list_style"
    | `Encoding -> "encoding"
end

type t = Yaml.value

let to_string t = Yaml.to_string_exn t

type value =
  | String of string option
  | ListStyle of ListStyle.t option
  | Encoding of Encoding.t option

let get_default = function
  | `StateDir -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | `LockFile -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | `Editor -> String (Sys.getenv "EDITOR")
  | `OnModification -> String None
  | `ListStyle -> ListStyle (Some `Fixed)
  | `Encoding -> Encoding (Some `Raw)

let value_of_string key s =
  match key with
  | `StateDir -> String (Some s)
  | `LockFile -> String (Some s)
  | `Editor -> String (Some s)
  | `OnModification -> String (Some s)
  | `ListStyle -> ListStyle (Some (ListStyle.of_string s))
  | `Encoding -> Encoding (Some (Encoding.of_string s))

let value_to_string value =
  match value with
  | String value -> ( match value with Some v -> v | None -> "" )
  | ListStyle value -> (
      match value with Some v -> ListStyle.to_string v | None -> "" )
  | Encoding value -> (
      match value with Some v -> Encoding.to_string v | None -> "" )

let get t key =
  match Ezjsonm.find_opt t [ Key.to_string key ] with
  | Some json -> value_of_string key (Ezjsonm.get_string json)
  | None -> get_default key

let set t key value =
  Ezjsonm.update t [ Key.to_string key ]
    (Some (Ezjsonm.string (value_to_string value)))

let get_string_opt t key =
  match get t key with
  | String value -> value
  | _ ->
      failwith
        (sprintf "BUG: you asked for a string but provided a %s"
           (Key.to_string key))

let get_string t key =
  match get_string_opt t key with
  | Some value -> value
  | None -> failwith (sprintf "%s not defined" (Key.to_string key))

let load =
  let cfg =
    match Sys.file_exists config_path with
    | `Yes -> Yaml.of_string_exn (In_channel.read_all config_path)
    | `No | `Unknown ->
        Unix.mkdir_p (Filename.dirname config_path);
        Out_channel.write_all config_path
          ~data:(Ezjsonm.to_string (Ezjsonm.dict []));
        Yaml.of_string_exn (In_channel.read_all config_path)
  in

  let state_dir = get_string cfg `StateDir in
  match Sys.file_exists state_dir with
  | `Yes -> cfg
  | `No | `Unknown ->
      Unix.mkdir_p state_dir;
      cfg

let save t = Out_channel.write_all ~data:(to_string t) config_path
