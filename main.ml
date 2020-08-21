open Core

let init_config path =
  let config_path =
    match path with Some path -> path | None -> Config.default_path
  in
  let config = Config.read_config config_path in
  Config.initialize config_path config;
  config

let create_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"create a new note"
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
    [%map_open
      let key =
        flag "get" (optional string) ~doc:"get a config value"
      in
      fun () ->
        let open Config in
        let cfg = init_config None in
        match key with
        | Some key -> print_string (get_exn cfg key)
        | None -> print_string (to_string cfg)]

let list_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list notes"
    ~readme:(fun () ->
      "\n\
       The list subcommand will list one or more notes stored\n\
       in the state directory, you can apply one or more filters\n\
       to reduce the number of results that are returned\n")
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
  Command.basic ~summary:"write a single note to stdout"
    ~readme:(fun () -> "\ncat a single note to stdout\n       ")
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
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let note = List.nth_exn notes 0 in
            print_endline (Note.to_string ~note)
        | _ -> failwith "too many results"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () -> "\nedit an existing note\n       ")
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
            let note, path = List.nth_exn notes 0 in
            Io.edit
              ~callback:(get cfg "on_modification")
              ~editor:(get_exn cfg "editor") path
        | _ -> failwith "too many results"]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () -> "\ndelete an existing note\n       ")
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
            let note, path = List.nth_exn notes 0 in
            (* TODO: prompt for confirmation *)
            Unix.remove path
        | _ -> failwith "too many results"]

let command =
  Command.group ~summary:"Note is a simple CLI based note taking application"
    [
      ("cat", cat_note);
      ("create", create_note);
      ("config", show_config);
      ("delete", delete_note);
      ("edit", edit_note);
      ("ls", list_notes);
    ]

let () = Command.run command
