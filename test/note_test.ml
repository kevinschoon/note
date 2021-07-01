open Core
open Note_lib

let parsing () =
  let n1 = {|
---
path: /fuu
tags: ["bar"]
description: "baz"
---
# Hello World|} in
  let n1 = n1 |> Note.of_string in
  Alcotest.(check string) "path" "/fuu" n1.frontmatter.path ;
  let tag = n1.frontmatter.tags |> List.hd_exn in
  Alcotest.(check string) "tag" "bar" tag ;
  let description = (Option.value_exn n1.frontmatter.description) in
  Alcotest.(check string) "description" "baz" description ;
  let content = n1.content in
  Alcotest.(check string) "content" "\n# Hello World" content


let load_manifest () =
  let state_dir = Filename.temp_dir "note-test" "" in
  let manifest = Manifest.load_or_init state_dir in
  let note_root =
    Note.of_string
      {|
---
path: "/"
description: "all notes desend from here"
tags: []
---
# Root Note
|}
  in
  let note_0 =
    Note.of_string
      {|
---
path: "/note-0"
description: "this is a note"
tags: ["fuu", "bar"]
---
|}
  in
  let note_1 =
    Note.of_string
      {|
---
path: "/note-0/note-1"
description: "this is another note"
tags: ["baz", "qux"]
---
|}
  in
  let manifest = manifest |> Manifest.create ~path:"/" in
  let item = manifest.items |> List.hd_exn in
  let slug = item.slug |> Slug.to_string in
  Out_channel.write_all ~data:(note_root |> Note.to_string) slug;
  let manifest = manifest |> Manifest.create ~path:"/note-0" in
  let item = manifest.items |> List.hd_exn in
  let slug = item.slug |> Slug.to_string in
  Out_channel.write_all ~data:(note_0 |> Note.to_string) slug;
  manifest |> Manifest.save;
  let manifest = manifest |> Manifest.create ~path:"/note-0/note-1" in
  let item = manifest.items |> List.hd_exn in
  let slug = item.slug |> Slug.to_string in
  Out_channel.write_all ~data:(note_1 |> Note.to_string) slug;
  manifest |> Manifest.save;
  let manifest = Manifest.load_or_init state_dir in
  let root =
    Note.Tree
      ( Note.of_string ~path:(Some "/") Note.root_template,
        Note.resolve_manifest ~path:"/" manifest )
  in
  let (Note.Tree (_, others)) = root in
  Alcotest.(check int) "one" 1 (others |> List.length)

let adapter () =
  let options : Note.Adapter.options =
    {
      state_dir = Filename.temp_dir "note-test" "";
      editor = "true";
      on_modification = None;
    }
  in
  let tree = options |> Note.Adapter.load ~path:"/" in
  Alcotest.(check int)
    "initialized" 1
    (tree |> Note.flatten ~accm:[] |> List.length);
  options |> Note.Adapter.create ~content:(Some "bar") ~path:"/fuu";
  let tree = options |> Note.Adapter.load ~path:"/" in
  Alcotest.(check int)
    "note added" 2
    (tree |> Note.flatten ~accm:[] |> List.length);
  options |> Note.Adapter.remove ~path:"/fuu";
  let tree = options |> Note.Adapter.load ~path:"/" in
  Alcotest.(check int)
    "note removed" 1
    (tree |> Note.flatten ~accm:[] |> List.length)

let () =
  Alcotest.run "Note"
    [
      ("parse", [ Alcotest.test_case "parse" `Quick parsing ]);
      ( "load_manifest",
        [ Alcotest.test_case "load manifest" `Quick load_manifest ] );
      ("adapter", [ Alcotest.test_case "adapter" `Quick adapter ]);
    ]
