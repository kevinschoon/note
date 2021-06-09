open Core
open Note_lib

let cfg = Config.config_path |> Config.load
let context = cfg.context

module Util = struct
  open ANSITerminal

  let rec to_words (accm : string list) (doc : Omd.doc) : string list =
    let split_words inline =
      match inline with Omd.Text text -> String.split ~on:' ' text | _ -> []
    in
    match doc with
    | [] -> accm
    | hd :: tl -> (
        (* TODO: account for headings, lists, etc *)
        match hd.bl_desc with
        | Paragraph inline ->
            let accm = accm @ split_words inline.il_desc in
            to_words accm tl
        | _ -> to_words accm tl)

  let paint_tag (styles : Config.StylePair.t list) text : string =
    match
      List.find ~f:(fun entry -> String.equal entry.pattern text) styles
    with
    | Some entry -> sprintf entry.styles "%s" text
    | None -> sprintf [ Foreground Default ] "%s" text

  let to_cells ~columns ~styles (notes : Note.note list) =
    let header =
      List.map
        ~f:(fun column ->
          let text_value = Config.Column.to_string column in
          let text_length = String.length text_value in
          let text_value = sprintf [ Bold; Underlined ] "%s" text_value in
          (text_value, text_length, 1))
        columns
    in
    let note_cells =
      let default_padding = 1 in
      List.fold ~init:[]
        ~f:(fun accm note ->
          accm
          @ [
              List.map
                ~f:(fun column ->
                  match column with
                  | `Title ->
                      let text_value = note.frontmatter.title in
                      (text_value, String.length text_value, default_padding)
                  | `Description ->
                      let text_value = note.frontmatter.description in
                      (text_value, String.length text_value, default_padding)
                  | `Slug ->
                      let text_value =
                        match note.slug with
                        | Some slug -> slug |> Slug.shortname
                        | None -> "??"
                      in
                      (text_value, String.length text_value, default_padding)
                  | `Tags ->
                      let text_value =
                        String.concat ~sep:"|" note.frontmatter.tags
                      in
                      let text_length = String.length text_value in
                      let tags = note.frontmatter.tags in
                      let tags =
                        List.map ~f:(fun tag -> paint_tag styles tag) tags
                      in
                      let text_value = String.concat ~sep:"|" tags in
                      (text_value, text_length, default_padding)
                  | `WordCount ->
                      let text_value =
                        Core.sprintf "%d"
                          (List.length
                             (to_words [] (note.content |> Omd.of_string)))
                      in
                      (text_value, String.length text_value, default_padding))
                columns;
            ])
        notes
    in
    [ header ] @ note_cells
end

module Encoding = struct
  let to_string ~style (note : Note.note) =
    match style with
    | `Raw -> note.content
    | `Json -> Ezjsonm.to_string (Note.to_json note)
    | `Yaml -> Yaml.to_string_exn (Note.to_json note)
    | `Html -> note.content |> Omd.of_string |> Omd.to_html
end

let note_of_title title =
  sprintf {|
---
title: "%s"
---

# %s
|} title title |> Note.of_string

let rec convert_tree tree =
  let (Note.Tree (note, others)) = tree in
  let title = note.frontmatter.title in
  let title = "[" ^ title ^ "]" in
  Display.Tree (title, List.map ~f:convert_tree others)

let get_notes =
  let notes = cfg.state_dir |> Note.load ~context |> Note.flatten ~accm:[] in
  notes

let get_title (note : Note.note) = note.frontmatter.title

let get_tags (note : Note.note) = note.frontmatter.tags

let to_keys ~kind notes =
  match kind with
  | `Title -> List.map ~f:get_title notes
  | `Tags -> List.concat (List.map ~f:get_tags notes)

let search_arg kind =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let notes = get_notes in
      List.filter_map
        ~f:(fun key ->
          if String.is_substring ~substring:part key then Some key else None)
        (to_keys ~kind notes))
    (fun filter -> filter)

let key_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys = List.map ~f:Config.Key.to_string Config.Key.all in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Config.Key.of_string

let flag_to_op state =
  match state with true -> Note.Operator.And | false -> Note.Operator.Or

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

let term_args =
  let open Command.Let_syntax in
  [%map_open
    let title =
      flag "title"
        (listed (search_arg `Title))
        ~doc:"regular expression matching the note title"
    and tags =
      flag "tag"
        (listed (search_arg `Tags))
        ~doc:"sequence of regular expressions matching note tags"
    in
    let term : Note.Term.t = { title; description = []; tags } in
    term]

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
      let term = term_args
      and encoding =
        flag "encoding"
          (optional_with_default cfg.encoding encoding_arg)
          ~doc:"format [json | yaml | raw] (default: raw)"
      in
      fun () ->
        let notes =
          cfg.state_dir |> Note.load ~context |> Note.find_many ~term ~notes:[]
        in
        List.iter
          ~f:(fun note ->
            print_endline (Encoding.to_string ~style:encoding note))
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
      and title = anon ("title" %: string) in
      fun () ->
        let slug = Slug.next cfg.state_dir in
        match open_stdin with
        | Some _ ->
            (* reading from stdin so write directly to note *)
            let note =
              In_channel.stdin |> In_channel.input_all |> Note.of_string
            in
            slug.path
            |> Io.create ~callback:cfg.on_modification
                 ~content:(Note.to_string note)
        | None ->
            let content = title |> note_of_title |> Note.to_string in
            Io.create_on_change ~callback:cfg.on_modification ~editor:cfg.editor
              ~content slug.path]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () ->
      {|
Delete the first note that matches the filter criteria.
|})
    [%map_open
      let term = term_args in
      fun () ->
        let note = cfg.state_dir |> Note.load ~context |> Note.find_one ~term in
        match note with
        | Some note ->
            (Option.value_exn note.slug).path
            |> Io.delete ~callback:cfg.on_modification
                 ~title:note.frontmatter.title
        | None -> failwith "not found"]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      {|
Select a note that matches the filter criteria and open it in your text editor.
|})
    [%map_open
      let term = term_args in
      fun () ->
        let note = cfg.state_dir |> Note.load ~context |> Note.find_one ~term in
        match note with
        | Some note ->
            (Option.value_exn note.slug).path
            |> Io.edit ~callback:cfg.on_modification ~editor:cfg.editor
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
      let term = term_args
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
        let notes = Note.find_many ~term ~notes:[] (Note.load ~context cfg.state_dir) in
        let styles = cfg.styles in
        let cells = Util.to_cells ~columns ~styles notes in
        Display.to_stdout ~style cells]

let sync =
  Command.basic ~summary:"sync notes to a remote server"
    (Command.Param.return (fun () -> Sync.sync cfg.on_sync))

let tree =
  Command.basic ~summary:"tree debug command"
    (Command.Param.return (fun () ->
         cfg.state_dir |> Note.load ~context |> convert_tree |> Display.to_string
         |> print_endline))

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
         ("tree", tree);
       ])
