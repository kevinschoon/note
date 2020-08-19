open Core

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
        let content = match open_stdin with
        | Some _ ->
            (In_channel.input_all In_channel.stdin)
        | None -> "" in
        let note : Note.t =
          { title; tags; content = content; created = Time.now () }
        in
        print_endline (Note.to_string note)]

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
        (anon ("name" %: string))
        ~f:(fun _ () ->
          let cfg = Config.read in
          Config.initialize cfg;
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
        let notes = Note.read_notes_filtered "db" filters in
        List.iter ~f:(fun x -> print_endline x.title) notes]

let command =
  Command.group ~summary:"list"
    [ ("create", create_note); ("config", show_config); ("list", list_notes) ]

let () = Command.run command
