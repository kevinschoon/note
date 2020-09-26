open Core
open Config

let get_notes =
  List.map
    ~f:(fun slug ->
      let data = In_channel.read_all (Slug.get_path slug) in
      Note.of_string ~data slug)
    (Slug.load (get_string load StateDir))

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
          (optional_with_default Encoding.Raw
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
            print_endline
              ( match encoding with
              | Json -> Ezjsonm.to_string (Note.to_json note)
              | Yaml -> Yaml.to_string_exn (Note.to_json note)
              | Raw -> In_channel.read_all (Note.get_path note) ))
          notes]

let show_config =
  let open Command.Let_syntax in
  Command.basic ~summary:"display the configuration"
    ~readme:(fun () ->
      {| 
Display the current configuration as inferred by Note. It is also possible to 
extract specific values by specifying a key value.

Examples

# display the current configuration
note config
# extract a specific value from the configuration
note config -get state_dir
|})
    [%map_open
      let key =
        flag "get"
          (optional (Command.Arg_type.create Key.of_string))
          ~doc:"get a config value"
      in
      fun () ->
        match key with
        | Some key ->
            let value = get load key in
            print_endline (value_as_string value)
        | None -> print_endline (to_string load)]

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
        let slug = Slug.next (get_string cfg Key.StateDir) in
        match open_stdin with
        | Some _ ->
            (* reading from stdin so write directly to note *)
            let content = In_channel.input_all In_channel.stdin in
            let note = Note.build ~tags ~content ~title slug in
            Io.create
              ~callback:(get_string_opt cfg Key.OnModification)
              ~content:(Note.to_string note) (Slug.get_path slug)
        | None ->
            let note = Note.build ~tags ~content:"" ~title slug in
            let init_content = Note.to_string note in
            Io.create_on_change
              ~callback:(get_string_opt cfg Key.OnModification)
              ~editor:(get_string cfg Key.Editor)
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
              ~callback:(get_string_opt load Key.OnModification)
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
              ~callback:(get_string_opt cfg Key.OnModification)
              ~editor:(get_string cfg Key.Editor)
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
          (optional_with_default ListStyle.Fixed
             (Arg_type.create ListStyle.of_string))
          ~doc:"list style [fixed | wide | simple]"
      in
      fun () ->
        let open Note.Filter in
        let filter_kind = if fulltext then Some Fulltext else None in
        let notes =
          Note.Filter.find_many ?strategy:filter_kind ~args:filter_args
            get_notes
        in
        let style =
          match style with
          | ListStyle.Fixed -> `Fixed
          | ListStyle.Wide -> `Wide
          | ListStyle.Simple -> `Simple
        in
        print_short ~style notes]

let run =
  Command.run ~version:"%%VERSION%%"
    (Command.group ~summary:"Note is a simple CLI based note taking application"
       [
         ("cat", cat_note);
         ("create", create_note);
         ("config", show_config);
         ("delete", delete_note);
         ("edit", edit_note);
         ("ls", list_notes);
       ])
