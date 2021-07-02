open Core
open Note_lib

(* todo global locking *)

let cfg = Config.config_path |> Config.load

let options : Note.options =
  {
    state_dir = cfg.state_dir;
    on_modification = cfg.on_modification;
    editor = cfg.editor;
  }

module Args = struct
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

  let config_key =
    Command.Arg_type.create
      ~complete:(fun _ ~part ->
        let string_keys = List.map ~f:Config.Key.to_string Config.Key.all in
        List.filter
          ~f:(fun key -> String.is_substring ~substring:part key)
          string_keys)
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
      let paths = anon (sequence ("path" %: Args.path)) in
      fun () ->
        let paths = match paths with [] -> [ "/" ] | paths -> paths in
        paths
        |> List.map ~f:(fun path -> options |> Note.load ~path)
        |> List.iter ~f:(fun notes ->
               let note = notes |> Note.fst in
               note |> Note.to_string |> print_endline)]

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
      let paths = anon (sequence ("path" %: Args.path)) in
      fun () ->
        let paths = match paths with [] -> [ "/" ] | paths -> paths in
        paths
        |> List.map ~f:(fun path -> options |> Note.load ~path)
        |> List.iter ~f:(fun notes ->
               notes |> Display.convert_tree |> Display.Hierarchical.to_string
               |> print_endline)]

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
