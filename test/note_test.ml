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
  Alcotest.(check string) "path" "/fuu" (n1 |> Note.frontmatter).path ;
  let tag = (n1 |> Note.frontmatter).tags |> List.hd_exn in
  Alcotest.(check string) "tag" "bar" tag ;
  let description = (Option.value_exn (n1 |> Note.frontmatter).description) in
  Alcotest.(check string) "description" "baz" description ;
  let content = (n1 |> Note.content) in
  Alcotest.(check string) "content" "\n# Hello World" content


let adapter () =
  let options : Note.options =
    {
      state_dir = Filename.temp_dir "note-test" "";
      editor = "true";
      on_modification = None;
    }
  in
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int)
    "initialized" 1
    (tree |> Note.flatten ~accm:[] |> List.length);
  options |> Note.create ~content:(Some "bar") ~path:"/fuu";
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int)
    "note added" 2
    (tree |> Note.flatten ~accm:[] |> List.length);
  options |> Note.remove ~path:"/fuu";
  let tree = options |> Note.load ~path:"/" in
  Alcotest.(check int)
    "note removed" 1
    (tree |> Note.flatten ~accm:[] |> List.length)

let () =
  Alcotest.run "Note"
    [
      ("parse", [ Alcotest.test_case "parse" `Quick parsing ]);
      ("adapter", [ Alcotest.test_case "adapter" `Quick adapter ]);
    ]
