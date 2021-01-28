open Core
open Config

let get_notes =
  List.map
    ~f:(fun slug ->
      let data = In_channel.read_all (Slug.get_path slug) in
      Note.of_string ~data slug)
    (Slug.load (get_string load `StateDir))

let filter_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let notes = get_notes in
      List.filter_map
        ~f:(fun note ->
          let title = Note.get_title note in
          if String.equal part "" then Some title
          else if String.is_substring ~substring:part title then Some title
          else None)
        notes)
    (fun filter -> filter)

let key_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys = List.map ~f:Key.to_string Key.all in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Key.of_string

(*
 * commands
 *)

let cat_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"write a note to stdout"
    ~readme:(fun () ->
      {|
Write one or more notes to stdout. By default the cat command will write every note 
to stdout as plain text however the encoding can be adjusted to `yaml` or `json` 
for consumption by other tools. 

Examples:

# print the parsed content of the fuubar note
note cat fuubar
# write all commands as a json list
note cat -encoding json
|})
    [%map_open
      let filter_args = anon (sequence ("filter" %: filter_arg))
      and fulltext =
        flag "fulltext" no_arg
          ~doc:"perform a fulltext search instead of just key comparison"
      and encoding =
        flag "encoding"
          (optional_with_default
             (Encoding.of_string (value_to_string (get load `Encoding)))
             (Command.Arg_type.create Encoding.of_string))
          ~doc:"format [json | yaml | raw] (default: raw)"
      in
      fun () ->
        let open Note.Filter in
        let filter_kind = if fulltext then Some Fulltext else None in
        let notes =
          find_many ?strategy:filter_kind ~args:filter_args get_notes
        in
        List.iter
          ~f:(fun note ->
            print_endline (Note.Encoding.to_string ~style:encoding note))
          notes]

let config_show =
  Command.basic ~summary:"show the current configuration"
    (Command.Param.return (fun () -> print_string (to_string load)))

let config_get =
  let open Command.Let_syntax in
  Command.basic ~summary:"get a config value"
    [%map_open
      let key = anon ("key" %: key_arg) in
      fun () -> print_endline (value_to_string (get load key))]

let config_set =
  let open Command.Let_syntax in
  Command.basic ~summary:"set a config value"
    [%map_open
      let key = anon ("key" %: key_arg) and value = anon ("value" %: string) in
      fun () ->
        let cfg = load in
        let cfg = set cfg key (value_of_string key value) in
        save cfg]

let create_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"create a new note"
    ~readme:(fun () ->
      {|
Create a new note and save it to disk in your configured `state_dir`. 
The `on_modification` call back will be invoked if the file is committed to disk.

Examples:

# create a new note with the given title and tags
note create "Remember The Milk" groceries fuu bar
# create a note by reading from stdin
note create -stdin <<EOF
# My Important Note

Hello World!
EOF
# the title will be inferred from the heading
note ls "My Important Note"
|})
    [%map_open
      let open_stdin =
        flag "stdin" (optional bool)
          ~doc:"read content from stdin and copy it into the note body"
      and title = anon ("title" %: string)
      and tags = anon (sequence ("tag" %: string)) in
      fun () ->
        let cfg = load in
        let slug = Slug.next (get_string cfg `StateDir) in
        match open_stdin with
        | Some _ ->
            (* reading from stdin so write directly to note *)
            let content = In_channel.input_all In_channel.stdin in
            let note = Note.build ~tags ~content ~title slug in
            Io.create
              ~callback:(get_string_opt cfg `OnModification)
              ~content:(Note.to_string note) (Slug.get_path slug)
        | None ->
            let note = Note.build ~tags ~content:"" ~title slug in
            let init_content = Note.to_string note in
            Io.create_on_change
              ~callback:(get_string_opt cfg `OnModification)
              ~editor:(get_string cfg `Editor)
              init_content (Slug.get_path slug)]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () ->
      {|
Delete the first note that matches the filter criteria. The `on_modification` call back will be invoked if the note is deleted. 

Examples

# delete the note called fuubar
note delete fuubar
|})
    [%map_open
      let filter_args = anon (sequence ("filter" %: filter_arg))
      and fulltext =
        flag "fulltext" no_arg
          ~doc:"perform a fulltext search instead of just key comparison"
      in
      fun () ->
        let open Note.Filter in
        let filter_kind = if fulltext then Fulltext else Keys in
        let notes = get_notes in
        let note =
          Note.Filter.find_one ~strategy:filter_kind ~args:filter_args notes
        in
        match note with
        | Some note ->
            Io.delete
              ~callback:(get_string_opt load `OnModification)
              ~title:(Note.get_title note) (Note.get_path note)
        | None -> failwith "not found"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      {| 
Select a note that matches the filter criteria and open it in your `$EDITOR`. The `on_modification` call back will be invoked if the edited file differs from the original. 

Examples

# edit the fuubar note
note edit fuubar
|})
    [%map_open
      let filter_args = anon (sequence ("filter" %: filter_arg))
      and fulltext =
        flag "fulltext" no_arg
          ~doc:"perform a fulltext search instead of just key comparison"
      in
      fun () ->
        let cfg = load in
        let open Note.Filter in
        let filter_kind = if fulltext then Fulltext else Keys in
        let note = find_one ~strategy:filter_kind ~args:filter_args get_notes in
        match note with
        | Some note ->
            Io.edit
              ~callback:(get_string_opt cfg `OnModification)
              ~editor:(get_string cfg `Editor)
              (Note.get_path note)
        | None -> failwith "not found"]

let list_notes =
  let open Note.Display in
  let open Command.Let_syntax in
  Command.basic ~summary:"list notes"
    ~readme:(fun () ->
      {| 
List notes that match the filter criteria, if no filter criteria is given all notes will be listed

Examples

# list all notes
note ls
```
|})
    [%map_open
      let filter_args = anon (sequence ("filter" %: filter_arg))
      and fulltext =
        flag "fulltext" no_arg
          ~doc:"perform a fulltext search instead of just key comparison"
      and style =
        flag "style"
          (optional_with_default
             (ListStyle.of_string (value_to_string (get load `ListStyle)))
             (Arg_type.create ListStyle.of_string))
          ~doc:"list style [fixed | wide | simple]"
      and columns =
        flag "columns"
          (optional_with_default
             (Column.of_string_list (value_to_string (get load `ColumnList)))
             (Arg_type.create Column.of_string_list))
          ~doc:"columns to include in output"
      in
      fun () ->
        let open Note.Filter in
        let filter_kind = if fulltext then Some Fulltext else None in
        let notes =
          Note.Filter.find_many ?strategy:filter_kind ~args:filter_args
            get_notes
        in
        to_stdout ~columns: columns ~style notes]

let run =
  Command.run ~version:"%%VERSION%%"
    (Command.group ~summary:"Note is a simple CLI based note taking application"
       [
         ("cat", cat_note);
         ("create", create_note);
         ( "config",
           Command.group ~summary:"config management"
             [ ("show", config_show); ("get", config_get); ("set", config_set) ]
         );
         ("delete", delete_note);
         ("edit", edit_note);
         ("ls", list_notes);
       ])
