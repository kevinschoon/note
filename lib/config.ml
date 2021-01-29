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

module Column = struct
  type t = [ `Title | `Description | `Tags | `WordCount | `Slug ]

  let to_string = function
    | `Title -> "title"
    | `Description -> "description"
    | `Tags -> "tags"
    | `WordCount -> "words"
    | `Slug -> "slug"

  let of_string = function
    | "title" -> `Title
    | "description" -> `Description
    | "tags" -> `Tags
    | "words" -> `WordCount
    | "slug" -> `Slug
    | key -> failwith (sprintf "unsupported column type: %s" key)

  let to_string_list t = String.concat ~sep:"," (List.map ~f:to_string t)

  let of_string_list str = List.map ~f:of_string (String.split ~on:',' str)
end

module Key = struct
  type t =
    [ `StateDir
    | `LockFile
    | `Editor
    | `OnModification
    | `OnSync
    | `ListStyle
    | `Encoding
    | `ColumnList ]

  let all =
    [
      `StateDir;
      `LockFile;
      `Editor;
      `OnModification;
      `OnSync;
      `ListStyle;
      `Encoding;
      `ColumnList;
    ]

  let of_string = function
    | "state_dir" -> `StateDir
    | "lock_file" -> `LockFile
    | "editor" -> `Editor
    | "on_modification" -> `OnModification
    | "on_sync" -> `OnSync
    | "list_style" -> `ListStyle
    | "encoding" -> `Encoding
    | "column_list" -> `ColumnList
    | key -> failwith (sprintf "bad configuration key %s" key)

  let to_string = function
    | `StateDir -> "state_dir"
    | `LockFile -> "lock_file"
    | `Editor -> "editor"
    | `OnModification -> "on_modification"
    | `OnSync -> "on_sync"
    | `ListStyle -> "list_style"
    | `Encoding -> "encoding"
    | `ColumnList -> "column_list"
end

type t = Yaml.value

let to_string t = Yaml.to_string_exn t

type value =
  | String of string option
  | ListStyle of ListStyle.t option
  | Encoding of Encoding.t option
  | ColumnList of Column.t list option

let get_default = function
  | `StateDir -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | `LockFile -> String (Some (Filename.concat base_xdg_share_path "/note"))
  | `Editor -> String (Sys.getenv "EDITOR")
  | `OnModification -> String None
  | `OnSync -> String None
  | `ListStyle -> ListStyle (Some `Fixed)
  | `Encoding -> Encoding (Some `Raw)
  | `ColumnList -> ColumnList (Some [ `Title; `Tags; `WordCount; `Slug ])

let value_of_string key s =
  match key with
  | `StateDir -> String (Some s)
  | `LockFile -> String (Some s)
  | `Editor -> String (Some s)
  | `OnModification -> String (Some s)
  | `OnSync -> String (Some s)
  | `ListStyle -> ListStyle (Some (ListStyle.of_string s))
  | `Encoding -> Encoding (Some (Encoding.of_string s))
  | `ColumnList -> ColumnList (Some (Column.of_string_list s))

let value_to_string value =
  match value with
  | String value -> ( match value with Some v -> v | None -> "" )
  | ListStyle value -> (
      match value with Some v -> ListStyle.to_string v | None -> "" )
  | Encoding value -> (
      match value with Some v -> Encoding.to_string v | None -> "" )
  | ColumnList value -> (
      match value with Some v -> Column.to_string_list v | None -> "" )

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

  (* intiailize the state directory if it is missing *)
  let state_dir = get_string cfg `StateDir in
  match Sys.file_exists state_dir with
  | `Yes -> cfg
  | `No | `Unknown ->
      Unix.mkdir_p state_dir;
      cfg

let populate t =
  List.fold ~init:t ~f: (fun accm key ->
    let value = get accm key in
    set accm key value
  ) Key.all

let save t = Out_channel.write_all ~data:(to_string t) config_path
