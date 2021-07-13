open Core
open Note_lib

let cfg = Config.config_path |> Config.load

let options : Note.options =
  {
    state_dir = cfg.state_dir;
    on_modification = cfg.on_modification;
    editor = cfg.editor;
  }

module Display = struct
  include Note_lib.Display

  let noop text = text

  let header columns =
    columns
    |> List.map ~f:(fun column -> (column |> Config.Column.to_string, noop))

  let to_rows ~columns (notes : Note.t list) : row list =
    notes
    |> List.map ~f:(fun note ->
           columns
           |> List.map ~f:(fun column ->
                  match column with
                  | `Title ->
                      ( (note |> Note.frontmatter).path |> Filename.basename,
                        noop )
                  | `Description -> (
                      match (note |> Note.frontmatter).description with
                      | Some description -> (description, noop)
                      | None -> ("", noop))
                  | `Tags ->
                      ( (note |> Note.frontmatter).tags |> String.concat ~sep:" ",
                        noop )))

  let rec convert_tree tree =
    let (Note.Tree.Tree (note, others)) = tree in
    let title =
      "[" ^ ((note |> Note.frontmatter).path |> Filename.basename) ^ "]"
    in
    Hierarchical.Tree (title, List.map ~f:convert_tree others)

  let convert_rows ~columns tree : row list =
    let (Note.Tree.Tree (_, others)) = tree in
    others
    |> List.map ~f:(fun other ->
           let (Note.Tree.Tree (note, _)) = other in
           note)
    |> to_rows ~columns

  let to_stdout ~style ~columns notes =
    match style with
    | `Tree ->
        notes |> convert_tree |> Display.Hierarchical.to_string |> print_endline
    | `Simple ->
        notes
        |> convert_rows ~columns:[ `Title ]
        |> List.iter ~f:(fun row -> print_endline (fst (row |> List.hd_exn)))
    | `Fixed ->
        (columns |> header) :: (notes |> convert_rows ~columns)
        |> Tabular.fixed
        |> List.iter ~f:(fun row -> print_endline row)
    | `Wide ->
        let width, _ = ANSITerminal.size () in
        (columns |> header) :: (notes |> convert_rows ~columns)
        |> Tabular.wide ~width
        |> List.iter ~f:(fun row -> print_endline row)
end

module Encoding = struct
  let to_stdout ~encoding tree =
    match encoding with
    | `Raw ->
        let note = tree |> Note.Tree.fst in
        note |> Note.to_string |> print_endline
    | `Json ->
        tree |> Note.Tree.to_json |> Ezjsonm.wrap |> Ezjsonm.to_string
        |> print_endline
    | `Yaml ->
        tree |> Note.Tree.to_json |> Ezjsonm.wrap |> Yaml.to_string_exn
        |> print_endline
    | `Html -> 
        tree |> Note.Tree.to_html |> print_endline
end

module Args = struct
  let list_style =
    let styles =
      Config.ListStyle.all |> List.map ~f:Config.ListStyle.to_string
    in
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        styles
        |> List.filter ~f:(fun style ->
               style |> String.is_substring ~substring:part))
      (fun key -> key |> Config.ListStyle.of_string)

  let path =
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        options |> Note.Completion.suggest_paths ~hint:part)
      (fun filter -> filter)

  let tag =
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        options |> Note.Completion.suggest_tags ~hint:part)
      (fun filter -> filter)

  let encoding =
    let encodings =
      Config.Encoding.all |> List.map ~f:Config.Encoding.to_string
    in
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        encodings
        |> List.filter ~f:(fun encoding ->
               String.is_substring ~substring:part encoding))
      Config.Encoding.of_string

  let config_key =
    let keys = List.map ~f:Config.Key.to_string Config.Key.all in
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        keys
        |> List.filter ~f:(fun key -> String.is_substring ~substring:part key))
      Config.Key.of_string
end

(*
 * commands
 *)

let config_show =
  Command.basic ~summary:"show the current configuration"
    (Command.Param.return (fun () -> print_string (Config.to_string cfg)))

let config_get =
  let open Command.Let_syntax in
  Command.basic ~summary:"get a config value"
    [%map_open
      let key = anon ("key" %: Args.config_key) in
      fun () -> print_endline (Config.get cfg key)]

let config_set =
  let open Command.Let_syntax in
  Command.basic ~summary:"set a config value"
    [%map_open
      let key = anon ("key" %: Args.config_key)
      and value = anon ("value" %: string) in
      fun () ->
        let cfg = Config.set cfg key value in
        Config.save cfg]

let cat_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list existing notes"
    ~readme:(fun () ->
      {| 
List one or more notes that match the filter criteria, if no filter criteria 
is provided then all notes will be listed.
|})
    [%map_open
      let paths = anon (sequence ("path" %: Args.path))
      and encoding = flag "encoding" (optional Args.encoding) ~doc:"encoding" in
      fun () ->
        let paths = match paths with [] -> [ "/" ] | paths -> paths in
        let encoding =
          match encoding with Some encoding -> encoding | None -> `Raw
        in
        paths
        |> List.map ~f:(fun path -> options |> Note.load ~path)
        |> List.iter ~f:(Encoding.to_stdout ~encoding)]

let create_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"create a new note"
    ~readme:(fun () ->
      {|
Create a new note and save it to disk in your configured state_dir. The
on_modification callback will be invoked if the file is committed to disk.
|})
    [%map_open
      let stdin =
        flag "stdin" (optional bool)
          ~doc:"read content from stdin and copy it into the note body"
      and path = anon ("path" %: Args.path)
      and tags = flag "tag" (listed Args.tag) ~doc:"tag"
      and description =
        flag "description" (optional string) ~doc:"description"
      in
      fun () ->
        let content =
          match stdin with
          | Some _ -> Some (In_channel.stdin |> In_channel.input_all)
          | None -> None
        in
        options |> Note.create ~description ~tags ~content ~path]

let remove_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"remove an existing note"
    ~readme:(fun () -> {||})
    [%map_open
      let path = anon ("path" %: Args.path) in
      fun () ->
        let message =
          Format.sprintf "Are you sure you want to delete note %s?" path
        in
        match options |> Note.find ~path with
        | Some _ ->
            let callback () = options |> Note.remove ~path in
            Util.prompt ~callback message
        | None -> failwith "not found"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      {|
Select a note that matches the filter criteria and open it in your text editor.
|})
    [%map_open
      let path = anon ("path" %: Args.path)
      and _ = flag "tag" (listed Args.tag) ~doc:"tag"
      and _ =
        flag "description" (optional_with_default "" string) ~doc:"description"
      in
      fun () -> options |> Note.edit ~path]

let list_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list existing notes"
    ~readme:(fun () ->
      {| 
List one or more notes that match the filter criteria, if no filter criteria 
is provided then all notes will be listed.
|})
    [%map_open
      let paths = anon (sequence ("path" %: Args.path))
      and style =
        flag "style"
          (optional_with_default cfg.list_style Args.list_style)
          ~doc:"style"
      in
      fun () ->
        let paths = match paths with [] -> [ "/" ] | paths -> paths in
        let columns = cfg.column_list in
        paths
        |> List.map ~f:(fun path -> options |> Note.load ~path)
        |> List.iter ~f:(fun notes ->
               notes |> Display.to_stdout ~style ~columns)]

let sync =
  Command.basic ~summary:"sync notes to a remote server"
    (Command.Param.return (fun () -> Sync.sync cfg.on_sync))

let serve =
  Command.basic ~summary:"serve notes from an http server"
    (Command.Param.return (fun () -> ()))

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let run =
  Command.run ~version ~build_info:""
    (Command.group ~summary:"Note is a simple CLI based note taking application"
       [
         ("cat", cat_notes);
         ("create", create_note);
         ( "config",
           Command.group ~summary:"config management"
             [ ("show", config_show); ("get", config_get); ("set", config_set) ]
         );
         ("rm", remove_note);
         ("edit", edit_note);
         ("ls", list_notes);
         ("sync", sync);
         ("serve", serve);
       ])
