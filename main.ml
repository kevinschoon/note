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
      and open_editor =
        flag "edit" (optional bool)
          ~doc:"open the note in your $EDITOR after creation"
      and title = anon ("title" %: string)
      and tags = anon (sequence ("tag" %: string)) in
      fun () ->
        let cfg = (Config.get_exn (init_config (None))) in
        let slugs = Slug.of_dir (cfg "state_dir") in
        let content =
          match open_stdin with
          | Some _ -> In_channel.input_all In_channel.stdin
          | None -> ""
        in
        let note : Note.t = { title; tags; content; created = Time.now () } in
        let next = Slug.next slugs in
        let target_file = Filename.concat (cfg "state_dir") (Slug.to_string next) in
        let init_content = Note.to_string note in
        Io.create_edit_write init_content target_file]

let show_config =
  (*
     TODO: this lib is so deeply confusing to me I cannot
     understand how to simply write a command that takes
     no arguments and executes a function
  *)
  Command.basic ~summary:"display the configuration"
    ~readme:(fun () ->
      "\n\
       This config subcommand will display the currently loaded\n\
       configuration as JSON")
    Command.Param.(
      map
        (anon (sequence ("_" %: string)))
        ~f:(fun _ () ->
          let cfg = init_config None in
          print_endline (Config.to_string cfg)))

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
        let cfg = (Config.get_exn (init_config None)) in
        let slugs = Slug.of_dir (cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s -> Filename.concat (cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes = Note.filter (Note.read_notes paths) filters in
        List.iter ~f:(fun x -> print_endline x.title) notes]

let cat_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"write a single note to stdout"
    ~readme:(fun () -> "\ncat a single note to stdout\n       ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let cfg = (Config.get_exn (init_config None)) in
        let slugs = Slug.of_dir (cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s -> Filename.concat (cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes = Note.filter (Note.read_notes paths) filters in
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let note = List.nth_exn notes 0 in
            print_endline (Note.to_string note)
        | _ -> failwith "too many results"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () -> "\nedit an existing note\n       ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let cfg = (Config.get_exn (init_config None)) in
        let slugs = Slug.of_dir (cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s -> Filename.concat (cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes =
          Note.filter_with_paths (Note.read_notes_preserve_paths paths) filters
        in
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let path, note = List.nth_exn notes 0 in
            Io.edit path
        | _ -> failwith "too many results"]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () -> "\ndelete an existing note\n       ")
    [%map_open
      let filters = anon (sequence ("filter" %: string)) in
      fun () ->
        let cfg = (Config.get_exn (init_config None)) in
        let slugs = Slug.of_dir (cfg "state_dir") in
        let paths =
          List.map
            ~f:(fun s -> Filename.concat (cfg "state_dir") (Slug.to_string s))
            slugs
        in
        let notes =
          Note.filter_with_paths (Note.read_notes_preserve_paths paths) filters
        in
        match List.length notes with
        | 0 -> failwith "no note found"
        | 1 ->
            let path, note = List.nth_exn notes 0 in
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
