open Core

let home = Sys.home_directory ()

let base_xdg_config_path = Filename.concat home ".config"

let base_xdg_share_path = Filename.concat home ".local/share"

module ListStyle = struct
  type t = Fixed | Wide | Simple

  let to_string = function
    | Fixed -> "fixed"
    | Wide -> "wide"
    | Simple -> "simple"

  let of_string = function
    | "fixed" -> Fixed
    | "wide" -> Wide
    | "simple" -> Simple
    | key -> failwith key
end

module Encoding = struct
  type t = Json | Yaml | Raw

  let to_string = function Json -> "json" | Yaml -> "yaml" | Raw -> "simple"

  let of_string = function
    | "json" -> Json
    | "yaml" -> Yaml
    | "raw" -> Raw
    | _ -> failwith "unsupported encoding type"
end

type t = Yaml.value

type value =
  | String of string option
  | ListStyle of ListStyle.t option
  | Encoding of Encoding.t option

module Key = struct
  type t =
    | StateDir
    | LockFile
    | Editor
    | OnModification
    | ListStyle
    | Encoding

  let of_string = function
    | "state_dir" -> StateDir
    | "lock_file" -> LockFile
    | "editor" -> Editor
    | "on_modification" -> OnModification
    | "list_style" -> ListStyle
    | "encoding" -> Encoding
    | key -> failwith (sprintf "bad configuration key %s" key) 

  let to_string = function
    | StateDir -> "state_dir"
    | LockFile -> "lock_file"
    | Editor -> "editor"
    | OnModification -> "on_modification"
    | ListStyle -> "list_style"
    | Encoding -> "encoding"
end

let get_default = function
  | Key.StateDir -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | Key.LockFile -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | Key.Editor -> String (Sys.getenv "EDITOR")
  | Key.OnModification -> String None
  | Key.ListStyle -> ListStyle (Some ListStyle.Fixed)
  | Key.Encoding -> Encoding (Some Encoding.Raw)

let of_json key json =
  match key with
  | Key.StateDir -> String (Some (Ezjsonm.get_string json))
  | Key.LockFile -> String (Some (Ezjsonm.get_string json))
  | Key.Editor -> String (Some (Ezjsonm.get_string json))
  | Key.OnModification -> String (Some (Ezjsonm.get_string json))
  | Key.ListStyle ->
      ListStyle (Some (ListStyle.of_string (Ezjsonm.get_string json)))
  | Key.Encoding ->
      Encoding (Some (Encoding.of_string (Ezjsonm.get_string json)))

let to_string t = Ezjsonm.to_string (Ezjsonm.wrap t)

let get t key =
  match Ezjsonm.find_opt t [ Key.to_string key ] with
  | Some json -> of_json key json
  | None -> get_default key

let value_as_string value =
  match value with
  | String value -> ( match value with Some v -> v | None -> "" )
  | ListStyle value -> (
      match value with Some v -> ListStyle.to_string v | None -> "" )
  | Encoding value -> (
      match value with Some v -> Encoding.to_string v | None -> "" )

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
  let path =
    match Sys.getenv "NOTE_CONFIG" with
    | Some path -> path
    | None -> Filename.concat base_xdg_config_path "/note/config.yaml"
  in
  let cfg =
    match Sys.file_exists path with
    | `Yes -> Yaml.of_string_exn (In_channel.read_all path)
    | `No | `Unknown ->
        Unix.mkdir_p (Filename.dirname path);
        Out_channel.write_all path ~data:(Ezjsonm.to_string (Ezjsonm.dict []));
        Yaml.of_string_exn (In_channel.read_all path)
  in

  let state_dir = get_string cfg Key.StateDir in
  match Sys.file_exists state_dir with
  | `Yes -> cfg
  | `No | `Unknown ->
      Unix.mkdir_p state_dir;
      cfg
