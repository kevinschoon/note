open Core
open Note_lib

let load_manifest () =
  let state_dir = Filename.temp_dir "note-test" "" in
  let manifest = Manifest.load_or_init state_dir in
  let note_root = 
    Note.of_string {|
---
path: "/"
description: "all notes desend from here"
tags: []
---
# Root Note
|} in
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
  Io.create ~callback:None ~content:(note_root |> Note.to_string) slug ;
  let manifest = manifest |> Manifest.create ~path:"/note-0" in
  let item = manifest.items |> List.hd_exn in
  let slug = item.slug |> Slug.to_string in
  Io.create ~callback:None ~content:(note_0 |> Note.to_string) slug ;
  manifest |> Manifest.save ;
  let manifest = manifest |> Manifest.create ~path:"/note-0/note-1" in
  let item = manifest.items |> List.hd_exn in
  let slug = item.slug |> Slug.to_string in
  Io.create ~callback:None ~content:(note_1 |> Note.to_string) slug ;
  manifest |> Manifest.save ;
  let manifest = Manifest.load_or_init state_dir in
  let root = (Note.Tree ((Note.of_string ~path:(Some "/") Note.root_template), (Note.resolve_manifest ~path:"/" manifest))) in
  let (Note.Tree (_, others)) = root in
  Alcotest.(check int) "one" 1 (others |> List.length) 

let () =
  Alcotest.run "Note"
    [
      ( "load_manifest",
        [ Alcotest.test_case "load manifest" `Quick load_manifest ] );
    ]
