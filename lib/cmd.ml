open Core

let init_config path =
  let config_path =
    match path with Some path -> path | None -> Config.default_path
  in
  let config = Config.read_config config_path in
  Config.initialize config_path config;
  config

type encoding = Json | Yaml | Text

let encoding_argument =
  Command.Arg_type.create
    (fun encoding_str ->
       match encoding_str with
       | "Json" | "json" | "JSON" -> Json
       | "Yaml" | "yaml" | "YAML" -> Yaml
       | "Text" | "text" | "TEXT" -> Text
       | _ -> failwith "unsupported encoding type")

type value = Config of Config.t | Note of Note.t

let encode_value value = function
  | Json -> (
      match value with
      | Config config -> Ezjsonm.to_string (Config.to_json config)
      | Note note -> Ezjsonm.to_string (Note.to_json ~note) )
  | Yaml -> (
      match value with
      | Config config -> Yaml.to_string_exn (Config.to_json config)
      | Note note -> Yaml.to_string_exn (Note.to_json ~note) )
  | Text -> (
      match value with
      | Config config -> Config.to_string config
      | Note note -> Note.to_string ~note )

let create_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"create a new note"
    ~readme:(fun () ->
      "\n\
       This command will create a new note and save it to disk\n\
       If the on_modification option is configured, it will run\n\
       that callback if the new note is saved from your editor.\n\n\
       EXAMPLES:\n\n\
       # create a note with tags\n\
       note create \"Vim Commands\" programming linux fuu bar\n\n\
       # write a file from stdin into a new note\n\
       cat some_file.txt | note create -stdin \"Some File\" baz qux\n\
      \  ")
    [%map_open
      let open_stdin =
        flag "stdin" (optional bool)
          ~doc:"read content from stdin and copy it into the note body"
      and title = anon ("title" %: string)
      and tags = anon (sequence ("tag" %: string)) in
      fun () ->
        let open Config in
        let cfg = init_config None in
        let next_slug = Slug.next (Slug.load (get_exn cfg "state_dir")) in
        let target_file =
          Filename.concat (get_exn cfg "state_dir") (Slug.to_string next_slug)
        in
        match open_stdin with
        | Some _ ->
            (* reading from stdin so write directly to note *)
            let content = In_channel.input_all In_channel.stdin in
            let note = Note.build ~title ~tags ~content in
            Io.create
              ~callback:(get cfg "on_modification")
              ~content:(Note.to_string ~note) target_file
        | None ->
            let note = Note.build ~title ~tags ~content:"" in
            let init_content = Note.to_string ~note in
            Io.create_on_change
              ~callback:(get cfg "on_modification")
              ~editor:(get_exn cfg "editor") init_content target_file]

let show_config =
  let open Command.Let_syntax in
  Command.basic ~summary:"display the configuration"
    ~readme:(fun () ->
      "\n\
       This command will display the current configuration and may also\n\
       be used to retrieve a config value.\n\n\
       EXAMPLES:\n\n\
       # get the current configuration\n\
       note config\n\n\
       # get a specific value from the configuration\n\
       note config -get state_dir\n\n\n\
      \  ")
    [%map_open
      let key = flag "get" (optional string) ~doc:"get a config value" 
      and encoding = flag "encoding" (optional_with_default Json encoding_argument)  ~doc: "encoding" in
      fun () ->
        let open Config in
        let cfg = init_config None in
        match key with
        | Some key -> print_string (get_exn cfg key)
        | None -> print_endline (encode_value (Config cfg) encoding)]

let list_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list notes"
    ~readme:(fun () ->
      "\n\
       This command will list notes stored in the state_directory, filters may \
       be applied to limit the results\n\n\
       EXAMPLES:\n\n\
       # list all notes\n\
       note config\n\n\
       # list notes matching fuu or bar\n\
       note config fuu bar\n\n\
      \      ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let open Config in
        let cfg = init_config None in
        let slugs = Slug.load (get_exn cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s ->
              Filename.concat (get_exn cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes = Note.filter (Note.read_notes ~paths) filters in
        List.iter ~f:(fun x -> print_endline (Note.get_title x)) notes]

let cat_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"write a note to stdout"
    ~readme:(fun () ->
      "\n\
       This command will write a single note to stdout, if more than\n\
       one note is returned it will raise an exception.\n\n\
       EXAMPLES:\n\n\
       # write the fuubar note to stdout\n\
       note cat fuubar\n\
      \    ")
    [%map_open
      let filters = anon (sequence ("filter" %: string))
      and encoding = flag "encoding" (optional_with_default Text encoding_argument) ~doc: "encoding format"
      in
      fun () ->
        let open Config in
        let cfg = init_config None in
        let slugs = Slug.load (get_exn cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s ->
              Filename.concat (get_exn cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes = Note.filter (Note.read_notes ~paths) filters in
        List.iter
          ~f:(fun note -> print_endline (encode_value (Note note) encoding))
          notes]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      "\n\
       This command will select a note based on your filter criteria and open \
       it in your\n\
       configured $EDITOR. If the on_modification option is configured, it \
       will run that callback if the note\n\
       is modified. If none or more than one note is returned it will raise an \
       exception.\n\n\
       EXAMPLES:\n\n\
       # edit the fuubar note\n\
       note edit fuubar\n\n\
      \    ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let open Config in
        let cfg = init_config None in
        let slugs = Slug.load (get_exn cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s ->
              Filename.concat (get_exn cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes =
          Note.filter_with_paths (Note.read_notes_with_paths ~paths) filters
        in
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let _, path = List.nth_exn notes 0 in
            Io.edit
              ~callback:(get cfg "on_modification")
              ~editor:(get_exn cfg "editor") path
        | _ -> failwith "too many results"]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () ->
      "\n\
       This command will delete the note that matches the filter criteria. If \
       none or more than\n\
       one note is returned it will raise an exception.\n\n\
       EXAMPLES:\n\n\
       # delete the fuubar note\n\
       note delete fuubar\n\
      \    ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let open Config in
        let cfg = init_config None in
        let slugs = Slug.load (get_exn cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s ->
              Filename.concat (get_exn cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes =
          Note.filter_with_paths (Note.read_notes_with_paths ~paths) filters
        in
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let _, path = List.nth_exn notes 0 in
            (* TODO: prompt for confirmation *)
            Unix.remove path
        | _ -> failwith "too many results"]

let run =
  Command.run
    (Command.group ~summary:"Note is a simple CLI based note taking application"
       [
         ("cat", cat_note);
         ("create", create_note);
         ("config", show_config);
         ("delete", delete_note);
         ("edit", edit_note);
         ("ls", list_notes);
       ])
