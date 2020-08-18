open Core

let show_config = 
  Command.basic ~summary: "display the configuration"
    ~readme: (fun () -> "
This config subcommand will display the currently loaded
configuration as JSON")
    Command.Param.(
       map (anon (maybe ("name" %: string))) ~f:(
        fun name () ->
            let cfg = Config.read in
            (Config.initialize cfg) ;
            print_endline (Config.to_string cfg) ;
        )
    )

let list_notes =
  Command.basic ~summary: "list existing notes"
    ~readme: (fun () -> "
The list subcommand will list one or more notes stored
in the state directory, you can apply one or more filters
to reduce the number of results that are returned
")
    Command.Let_syntax.(
      let%map_open
        filters = anon ( sequence ("filter" %: string) )
      in
      fun () ->
        let notes = Note.read_notes_filtered "db" filters in
            (List.iter ~f: (fun x -> Note.display_note_fancy x) notes) ;
      )

let delete_note = 
    Command.basic ~summary: "Create a New Note"
    Command.Param.(
       map (anon (maybe ("name" %: string))) ~f:(
        fun name () -> 
            print_endline "creating a new note" ;
        )
    )

let command = Command.group ~summary:"list" [ 
        ("config", show_config) ;
        ("list", list_notes) ;
        ("delete", delete_note)
    ]

let () = 
    Command.run command
