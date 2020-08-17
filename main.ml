open Core

let list_notes =
  Command.basic ~summary:"List Notes"
    Command.Param.(
      map (anon (maybe ("filter" %: string))) ~f:(
        fun filter_str () -> 
            let notes = Note.read_tasks "db" in
            List.iter ~f: (fun x -> Note.display_task x) notes;
        )
    )


let edit_note = 
    Command.basic ~summary: "Create a New Note or Edit An Existing Note"
    Command.Param.(
       map (anon (maybe ("name" %: string))) ~f:(
        fun name () -> 
            print_endline "creating a new note" ;
        )       
    )


let delete_note = 
    Command.basic ~summary: "Create a New Note"
    Command.Param.(
       map (anon (maybe ("name" %: string))) ~f:(
        fun name () -> 
            print_endline "creating a new note" ;
        )       
    )

let command = Command.group ~summary:"list" [ ("list", list_notes) ]

let () = 
    Command.run command
