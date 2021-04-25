open Core

let cfg = Config.load

let get_notes =
  List.map
    ~f:(fun slug ->
      let content = In_channel.read_all (Slug.get_path slug) in
      Note.of_string ~content slug)
    (Slug.load cfg.state_dir)

let to_keys ~kind notes =
  match kind with
  | `Title -> List.map ~f:Note.get_title notes
  | `Tags -> List.concat (List.map ~f:Note.get_tags notes)

let search_arg kind =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let notes = get_notes in
      List.filter_map
        ~f:(fun key ->
          if String.is_substring ~substring:part key then Some key else None)
        (to_keys ~kind notes))
    (fun filter -> Re.Str.regexp filter)

let key_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys = List.map ~f:Config.Key.to_string Config.Key.all in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Config.Key.of_string

let flag_to_op state = match state with true -> Note.And | false -> Note.Or

let column_list_arg =
  Command.Arg_type.create (fun value ->
      List.map ~f:Config.Column.of_string (String.split ~on:',' value))

let encoding_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys =
        List.map ~f:Config.Encoding.to_string Config.Encoding.all
      in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Config.Encoding.of_string

let list_style_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys =
        List.map ~f:Config.ListStyle.to_string Config.ListStyle.all
      in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Config.ListStyle.of_string

let filter_args =
  let open Command.Let_syntax in
  [%map_open
    let titles =
      flag "title"
        (listed (search_arg `Title))
        ~doc:"regular expression matching the note title"
    and tags =
      flag "tag"
        (listed (search_arg `Tags))
        ~doc:"sequence of regular expressions matching note tags"
    and operator = flag "and" no_arg ~doc:"logical AND instead of default OR" in
    (titles, tags, operator)]

(*
 * commands
 *)

let cat_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"write notes to stdout"
    ~readme:(fun () ->
      {|
Write one or more notes to stdout. By default the cat command will write every 
note to stdout as plain text however the encoding can be adjusted to yaml or 
json for consumption by other tools.
|})
    [%map_open
      let titles, tags, operator = filter_args
      and encoding =
        flag "encoding"
          (optional_with_default cfg.encoding encoding_arg)
          ~doc:"format [json | yaml | raw] (default: raw)"
      in
      fun () ->
        let notes =
          Note.find_many
            ~term:{ titles; tags; operator = flag_to_op operator }
            get_notes
        in
        List.iter
          ~f:(fun note ->
            print_endline (Note.Encoding.to_string ~style:encoding note))
          notes]

let config_show =
  Command.basic ~summary:"show the current configuration"
    (Command.Param.return (fun () -> print_string (Config.to_string cfg)))

let config_get =
  let open Command.Let_syntax in
  Command.basic ~summary:"get a config value"
    [%map_open
      let key = anon ("key" %: key_arg) in
      fun () -> print_endline (Config.get cfg key)]

let config_set =
  let open Command.Let_syntax in
  Command.basic ~summary:"set a config value"
    [%map_open
      let key = anon ("key" %: key_arg) and value = anon ("value" %: string) in
      fun () ->
        let cfg = Config.set cfg key value in
        Config.save cfg]

let create_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"create a new note"
    ~readme:(fun () ->
      {|
Create a new note and save it to disk in your configured state_dir. The
on_modification callback will be invoked if the file is committed to disk.
|})
    [%map_open
      let open_stdin =
        flag "stdin" (optional bool)
          ~doc:"read content from stdin and copy it into the note body"
      and title = anon ("title" %: string)
      and tags = anon (sequence ("tag" %: string)) in
      fun () ->
        let slug = Slug.next cfg.state_dir in
        match open_stdin with
        | Some _ ->
            (* reading from stdin so write directly to note *)
            let content = In_channel.input_all In_channel.stdin in
            let note = Note.build ~tags ~content ~title slug in
            Io.create ~callback:cfg.on_modification
              ~content:(Note.to_string note) (Slug.get_path slug)
        | None ->
            let note = Note.build ~tags ~content:"" ~title slug in
            let init_content = Note.to_string note in
            Io.create_on_change ~callback:cfg.on_modification ~editor:cfg.editor
              init_content (Slug.get_path slug)]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () ->
      {|
Delete the first note that matches the filter criteria.
|})
    [%map_open
      let titles, tags, operator = filter_args in
      fun () ->
        let notes = get_notes in
        let note =
          Note.find_one
            ~term:{ titles; tags; operator = flag_to_op operator }
            notes
        in
        match note with
        | Some note ->
            Io.delete ~callback:cfg.on_modification ~title:(Note.get_title note)
              (Note.get_path note)
        | None -> failwith "not found"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      {|
Select a note that matches the filter criteria and open it in your text editor.
|})
    [%map_open
      let titles, tags, operator = filter_args in
      fun () ->
        let note =
          Note.find_one
            ~term:{ titles; tags; operator = flag_to_op operator }
            get_notes
        in
        match note with
        | Some note ->
            Io.edit ~callback:cfg.on_modification ~editor:cfg.editor
              (Note.get_path note)
        | None -> failwith "not found"]

let list_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list existing notes"
    ~readme:(fun () ->
      {| 
List one or more notes that match the filter criteria, if no filter criteria 
is provided then all notes will be listed.
|})
    [%map_open
      let titles, tags, operator = filter_args
      and style =
        flag "style"
          (optional_with_default cfg.list_style list_style_arg)
          ~doc:"list style [fixed | wide | simple]"
      and columns =
        flag "columns"
          (optional_with_default cfg.column_list column_list_arg)
          ~doc:"columns to include in output"
      in
      fun () ->
        let notes =
          Note.find_many
            ~term:{ titles; tags; operator = flag_to_op operator }
            get_notes
        in
        let styles = cfg.styles in
        let cells = Note.to_cells ~columns ~styles notes in
        Display.to_stdout ~style cells]

let sync =
  Command.basic ~summary:"sync notes to a remote server"
    (Command.Param.return (fun () -> Sync.sync cfg.on_sync))

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let run =
  Command.run ~version ~build_info:""
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
         ("sync", sync);
       ])
