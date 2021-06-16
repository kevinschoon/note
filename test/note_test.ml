open Core
open Note_lib

let dump_notes (notes : Note.note list) =
  List.iter ~f:(fun note -> print_endline note.frontmatter.title) notes

let rec convert_tree tree =
  let (Note.Tree (note, others)) = tree in
  let title = note.frontmatter.title in
  let title = "[" ^ title ^ "]" in
  Display.Hierarchical.Tree (title, List.map ~f:convert_tree others)

let make_a_note () =
  let note =
    Note.of_string
      {|
---
title: this is a note
description: although it doesn't contain anything of value
tags: ["it", "is", "still", "a", "note"]
---

# What is the Purpose of Life?

```json
{
  "answer": "it isn't clear"
}
```
  |}
  in

  let title = note.frontmatter.title in
  Alcotest.(check string) "title" "this is a note" title;
  let description = note.frontmatter.description in
  Alcotest.(check string)
    "description" "although it doesn't contain anything of value" description;
  let tags = note.frontmatter.tags in
  Alcotest.(check (list string))
    "tags"
    [ "it"; "is"; "still"; "a"; "note" ]
    tags;
  let data =
    Ezjsonm.find (note |> Note.to_json) [ "data" ]
    |> Ezjsonm.get_list (fun x -> x)
    |> List.hd_exn
  in
  let answer = Ezjsonm.find data [ "answer" ] |> Ezjsonm.get_string in
  Alcotest.(check string) "answer" "it isn't clear" answer

let note_of_title title =
  let template =
    sprintf {|
---
title: %s
---

# Note %s

Hello World |} title title
  in
  Note.of_string template

(*
[root]
├──[note-0]
├──[note-1]
│  └──[note-2]
├──[note-3]
├──[note-4]
│   ├──[note-5]
│   │   └──[note-6]
│   └──[note-7]
└──[note-8]
*)

let tree =
  Note.Tree
    ( note_of_title "root",
      [
        Note.Tree (note_of_title "note-0", []);
        Note.Tree
          (note_of_title "note-1", [ Note.Tree (note_of_title "note-2", []) ]);
        Note.Tree (note_of_title "note-3", []);
        Note.Tree
          ( note_of_title "note-4",
            [
              Note.Tree
                ( note_of_title "note-5",
                  [ Note.Tree (note_of_title "note-6", []) ] );
              Note.Tree (note_of_title "note-7", []);
            ] );
        Note.Tree (note_of_title "note-8", []);
      ] )

let find_many () =
  let results =
    Note.find_many
      ~term:{ title = [ "note-3"; "note-4" ]; description = []; tags = [] }
      ~notes:[] tree
  in
  dump_notes results;
  let n_results = results |> List.length in
  Alcotest.(check int) "two results" 2 n_results;
  let n3 = List.nth_exn results 1 in
  Alcotest.(check string) "first result" "note-3" n3.frontmatter.title;
  let n4 = List.nth_exn results 0 in
  Alcotest.(check string) "first result" "note-4" n4.frontmatter.title

let insert_at () =
  let n9 = note_of_title "note-9" in
  let tree, inserted =
    Note.insert
      ~term:(Some { title = [ "note-3" ]; description = []; tags = [] })
      ~tree
      (Note.Tree (n9, []))
  in
  Alcotest.(check bool) "inserted" true inserted ;
  let result =
    Note.find_one
      ~term:{ title = [ "note-9" ]; description = []; tags = [] }
      tree
  in
  Alcotest.(check bool) "inserted" true (Option.is_some result)

let test_structure () =
  let expected =
    {|
[root]
├──[note-0]
├──[note-1]
│  └──[note-2]
├──[note-3]
├──[note-4]
│  ├──[note-5]
│  │  └──[note-6]
│  └──[note-7]
└──[note-8]
|}
  in
  Alcotest.(check int) "length" 9 (Note.length tree);
  let note_tree = tree |> convert_tree |> Display.Hierarchical.to_string in
  Alcotest.(check string) "structure" expected note_tree

(*
we are attempting to go from a flat list of things that may or may
not reference other items from within the list and construct a hiarchrial
tree.

Example:

[
  note-1
  note-2 [ref note-4]
  note-3
  note-4 [ref note-1]
  note-5
]

.
├──[note-1]
│   │──[note-4]
│      └──[note-2]
│note-3
└note-5


def resolve(tree, notes):
  buf = []
  for note in notes:
    if note.ref == None:
      tree.add(note)
    else:
      inserted = tree.insert(note.ref)
      if not inserted:
        buf.append(note)
  if buf.length > 0:
    return resolve(tree, buf)
  return tree

*)
let n0 = Note.of_string {| 
  ---
  title: note-0
  ---
  # Note 0
  |}

let n1 =
  Note.of_string
    {|
  ---
  title: note-1
  parent: {"title": ["note-0"]}
  ---
  # Note 1
  |}

let n2 = Note.of_string {|
  ---
  title: note-2
  ---
  # Note 2
  |}

let n3 =
  Note.of_string
    {|
  ---
  title: note-3
  parent: {"title": ["note-1"]}
  ---
  # Note 3
|}

let n4 = 
  Note.of_string {|
  ---
  title: note-4
  parent: {"title": ["note-1"]}
  ---
  # Note 4
|}

let test_buf_insert () =
  let root = Note.Tree (Note.of_string Note.root_template, []) in
  let tree, buf = Note.buf_insert ~root [ n3; n2; n1; n0 ] in
  Alcotest.(check int) "n" 2 (List.length buf);
  let _, buf = Note.buf_insert ~root:tree buf in
  Alcotest.(check int) "n" 0 (List.length buf)

let test_resolve () =
  let root = Note.Tree (Note.of_string Note.root_template, []) in
  let expected =
    {|
[root]
├──[note-2]
└──[note-0]
   └──[note-1]
      ├──[note-4]
      └──[note-3]
|}
  in
  let tree_as_string =
    [ n3; n2; n1; n0 ; n4] |> Note.resolve ~root |> convert_tree
    |> Display.Hierarchical.to_string
  in
  Alcotest.(check string) "resolve" expected tree_as_string

let () =
  Alcotest.run "Note"
    [
      ( "create",
        [ Alcotest.test_case "create a note from a string" `Quick make_a_note ]
      );
      ( "find-many",
        [
          Alcotest.test_case "find notes with multiple criteria" `Quick
            find_many;
        ] );
      ( "insert",
        [ Alcotest.test_case "insert a note into a tree" `Quick insert_at ] );
      ( "structure",
        [ Alcotest.test_case "note tree structure" `Quick test_structure ] );
      ( "buf_insert",
        [ Alcotest.test_case "buf insert flat list" `Quick test_buf_insert ] );
      ("resolve", [ Alcotest.test_case "resolve flat list" `Quick test_resolve ]);
    ]
