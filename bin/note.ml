open Core
open Note_lib

let cfg = Config.config_path |> Config.load

let manifest = cfg.state_dir |> Manifest.load_or_init

let root = match manifest |> Manifest.find ~path:"/" with
  | Some item -> (item.slug |> Slug.to_string |> In_channel.read_all |> Note.of_string)
  | None -> 
      let manifest = manifest |> Manifest.create ~path:"/" in
      let last = manifest.items |> List.hd_exn in
      let slug = last.slug |> Slug.to_string in
      let root = Note.root_template |> Note.of_string in
      slug |> Out_channel.write_all ~data: (root |> Note.to_string) ;
      manifest |> Manifest.save ;
      root

let notes = (Note.Tree (root, manifest |> Note.resolve_manifest ~path:"/"))

let get_title (note : Note.note) = note.frontmatter.path

let get_tags (note : Note.note) = note.frontmatter.tags

let to_keys ~kind notes =
  match kind with
  | `Title -> List.map ~f:get_title notes
  | `Tags -> List.concat (List.map ~f:get_tags notes)

let name_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part -> [ part ])
    (fun filter -> filter)

let tag_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part -> [ part ])
    (fun filter -> filter)

let key_arg =
  Command.Arg_type.create
    ~complete:(fun _ ~part ->
      let string_keys = List.map ~f:Config.Key.to_string Config.Key.all in
      List.filter
        ~f:(fun key -> String.is_substring ~substring:part key)
        string_keys)
    Config.Key.of_string

let last_slug = manifest.items |> List.map ~f:(fun item -> item.slug) |> List.hd

(*
 * commands
 *)

let cat_note =
  Command.basic ~summary:"show the current configuration"
    (Command.Param.return (fun () -> ()))

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
      let _ =
        flag "stdin" (optional bool)
          ~doc:"read content from stdin and copy it into the note body"
      and path = flag "path" (required name_arg) ~doc:"path"
      and tags = flag "tag" (listed tag_arg) ~doc:"tag"
      and description =
        flag "description" (optional string) ~doc:"description"
      in
      fun () ->
        let manifest = manifest |> Manifest.create ~path in
        let last = List.hd_exn manifest.items in
        let note : Note.note =
          {
            frontmatter = { path = last.path; description; tags };
            content = "";
          }
        in
        Io.create ~callback:None ~content:(note |> Note.to_string)
          (Slug.to_string last.slug);
        manifest |> Manifest.save]

let delete_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"delete an existing note"
    ~readme:(fun () ->
      {|
Delete the first note that matches the filter criteria.
|})
    [%map_open
      let path = flag "path" (required name_arg) ~doc:"path" in
      fun () ->
        let manifest = manifest |> Manifest.remove ~path in
        manifest |> Manifest.save]

let edit_note =
  let open Command.Let_syntax in
  Command.basic ~summary:"edit an existing note"
    ~readme:(fun () ->
      {|
Select a note that matches the filter criteria and open it in your text editor.
|})
    [%map_open
      let _ = flag "path" (required name_arg) ~doc:"path"
      and _ = flag "tag" (listed tag_arg) ~doc:"tag"
      and _ =
        flag "description" (optional_with_default "" string) ~doc:"description"
      in
      fun () -> ()]

let list_notes =
  let open Command.Let_syntax in
  Command.basic ~summary:"list existing notes"
    ~readme:(fun () ->
      {| 
List one or more notes that match the filter criteria, if no filter criteria 
is provided then all notes will be listed.
|})
    [%map_open
      let _ = anon (sequence ("path" %: string)) in
      fun () ->
        notes |> Display.convert_tree |> Display.Hierarchical.to_string
        |> print_endline
      (*
        let items =
          match paths |> List.length with
          | 0 -> [ manifest.items ]
          | _ ->
              paths |> List.map ~f:(fun path -> manifest |> Manifest.list ~path)
        in
        items
        |> List.iter ~f:(fun items ->
               items
               |> List.iter ~f:(fun item ->
                      print_endline
                        (item |> Manifest.Item.to_json |> Ezjsonm.to_string)))
          *)]

let sync =
  Command.basic ~summary:"sync notes to a remote server"
    (Command.Param.return (fun () -> Sync.sync cfg.on_sync))

let serve =
  Command.basic ~summary:"serve notes from an http server"
    (Command.Param.return (fun () -> ()))

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
         ("serve", serve);
       ])
