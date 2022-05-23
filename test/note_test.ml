open Core
open Note_lib

let parsing () =
  let n1 =
    {|
---
path: /fuu
tags: ["bar"]
description: "baz"
---
# Hello World|}
  in
  let n1 = n1 |> Note.of_string in
  Alcotest.(check string) "path" "/fuu" (n1 |> Note.frontmatter).path;
  let tag = (n1 |> Note.frontmatter).tags |> List.hd_exn in
  Alcotest.(check string) "tag" "bar" tag;
  let description = Option.value_exn (n1 |> Note.frontmatter).description in
  Alcotest.(check string) "description" "baz" description;
  let content = n1 |> Note.content in
  Alcotest.(check string) "content" "\n# Hello World" content

let adapter () =
  let options : Note.options =
    {
      state_dir = Filename_unix.temp_dir "note-test" "";
      editor = "true";
      on_modification = None;
    }
  in
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int) "initialized" 1 (tree |> Note.Tree.flatten |> List.length);
  options |> Note.create ~content:(Some "bar") ~path:"/fuu";
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int) "note added" 2 (tree |> Note.Tree.flatten |> List.length);
  options |> Note.remove ~path:"/fuu";
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int)
    "note removed" 1
    (tree |> Note.Tree.flatten |> List.length)

let suggest_path () =
  let options : Note.options =
    {
      state_dir = Filename_unix.temp_dir "note-test" "";
      editor = "true";
      on_modification = None;
    }
  in
  options |> Note.create ~content:(Some "fuu") ~path:"/fuu";
  options |> Note.create ~content:(Some "bar") ~path:"/fuu/bar";
  options |> Note.create ~content:(Some "baz") ~path:"/fuu/baz";
  let suggestions = options |> Note.Completion.suggest_paths ~hint:"/f" in
  let result = List.nth_exn suggestions 0 in
  Alcotest.(check string) "suggestion" "/fuu" result;
  let suggestions = options |> Note.Completion.suggest_paths ~hint:"/fuu/b" in
  let result = List.nth_exn suggestions 0 in
  Alcotest.(check string) "suggestion" "/fuu/baz" result;
  let result = List.nth_exn suggestions 1 in
  Alcotest.(check string) "suggestion" "/fuu/bar" result

let suggest_tags () =
  let options : Note.options =
    {
      state_dir = Filename_unix.temp_dir "note-test" "";
      editor = "true";
      on_modification = None;
    }
  in
  options |> Note.create ~tags:[ "aa"; "bb" ] ~content:(Some "") ~path:"/fuu";
  options |> Note.create ~tags:[ "cc"; "dd" ] ~content:(Some "") ~path:"/bar";
  let result = options |> Note.Completion.suggest_tags ~hint:"a" in
  Alcotest.(check string) "tag aa" "aa" (List.nth_exn result 0)

let structured_data () =
  let note = Note.of_string {|
# Some Data
```json
{"a": "b"}
```
|} in
  let result =
    note |> Note.to_json |> Ezjsonm.wrap |> Ezjsonm.to_string
    |> Ezjsonm.from_string
  in
  let result = Ezjsonm.get_list (fun a -> a) result in
  let result = List.nth_exn result 0 in
  let result =
    Ezjsonm.find result [ "data" ] |> Ezjsonm.get_list (fun a -> a)
  in
  let result = List.nth_exn result 0 in
  let result = Ezjsonm.find result [ "a" ] |> Ezjsonm.get_string in
  Alcotest.(check string) "data" "b" result

let () =
  Alcotest.run "Note"
    [
      ("parse", [ Alcotest.test_case "parse" `Quick parsing ]);
      ("adapter", [ Alcotest.test_case "adapter" `Quick adapter ]);
      ( "path_suggestion",
        [ Alcotest.test_case "suggest path" `Quick suggest_path ] );
      ( "tag_suggestion",
        [ Alcotest.test_case "suggest tags" `Quick suggest_tags ] );
      ( "structured",
        [ Alcotest.test_case "structured data" `Quick structured_data ] );
    ]
