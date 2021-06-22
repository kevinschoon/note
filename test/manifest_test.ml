open Core
open Note_lib

let test_recurse () =
  let manifest =
    Manifest.empty
    |> Manifest.insert ~path:"/a" ~slug:"note-00000000-0" ~description:""
         ~tags:[]
    |> Manifest.insert ~path:"/a/b" ~slug:"note-00000000-1" ~description:""
         ~tags:[]
    |> Manifest.insert ~path:"/a/b/c" ~slug:"note-00000000-2" ~description:""
         ~tags:[]
    |> Manifest.insert ~path:"/a/b/c/d" ~slug:"note-00000000-3" ~description:""
         ~tags:[]
  in
  Alcotest.(check int) "n_results" 4 (List.length manifest.items)

let test_manifest () =
  let temp_db = Filename.temp_file "note-test" "" in
  Manifest.empty |> Manifest.save ~path:temp_db;
  let manifest =
    Manifest.load_or_init temp_db
    |> Manifest.insert ~path:"/fuu" ~slug:"note-00000000-0.md" ~description:""
         ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu" in
  Alcotest.(check bool) "manifest loaded" (result |> Option.is_some) true;
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu/bar" ~slug:"note-00000000-1.md"
         ~description:"" ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu/bar" in
  Alcotest.(check bool)
    "manifest /fuu/bar inserted" (result |> Option.is_some) true;
  let result_path = Option.value_exn result |> Manifest.to_path ~manifest in
  Alcotest.(check string) "result path" "/fuu/bar" result_path;
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu/baz" ~slug:"note-00000000-2.md"
         ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu" in
  Alcotest.(check int) "n_results" 2 (List.length results);
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu/bar/qux" ~slug:"note-00000000-3.md"
         ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu/bar" in
  Alcotest.(check int) "n_results" 1 (List.length results);
  Alcotest.(check int) "n_items" 4 (List.length manifest.items);
  print_endline (Manifest.to_string manifest);
  let manifest = manifest |> Manifest.remove ~path:"/fuu/bar/qux" in
  print_endline (Manifest.to_string manifest);
  Alcotest.(check int) "remove" 3 (List.length manifest.items)

let test_update () =
  let manifest =
    Manifest.empty
    |> Manifest.insert ~path:"/a" ~slug:"note-00000000-0" ~description:""
         ~tags:[]
    |> Manifest.insert ~path:"/a/b" ~slug:"note-00000000-1" ~description:""
         ~tags:[]
  in
  Alcotest.(check int) "two entries" 2 (List.length manifest.items);
  let manifest =
    manifest |> Manifest.update ~path:"/a/b" ~description:"" ~tags:[ "a"; "b" ]
  in
  let result = Option.value_exn (manifest |> Manifest.find ~path:"/a/b") in
  Alcotest.(check string) "updated" "a" (List.nth_exn result.tags 0);
  Alcotest.(check string) "updated" "b" (List.nth_exn result.tags 1);

  Alcotest.(check int) "two entries" 2 (List.length manifest.items)

let test_move () =
  let manifest =
    Manifest.empty
    |> Manifest.insert ~path:"/a" ~slug:"note-00000000-0" ~description:""
         ~tags:[]
    |> Manifest.insert ~path:"/a/b" ~slug:"note-00000000-1" ~description:""
         ~tags:[]
  in
  let manifest =
    manifest
    |> Manifest.update ~new_path:(Some "/b") ~path:"/a/b" ~description:""
         ~tags:[]
  in
  Alcotest.(check bool)
    "moved" true
    (manifest |> Manifest.find ~path:"/b" |> Option.is_some) ;
  Alcotest.(check int) "two entries" 2 (List.length manifest.items)

let () =
  Alcotest.run "Config"
    [
      ("recurse", [ Alcotest.test_case "test recurse" `Quick test_recurse ]);
      ("load", [ Alcotest.test_case "test manifest" `Quick test_manifest ]);
      ("update", [ Alcotest.test_case "test update" `Quick test_update ]);
      ("move", [ Alcotest.test_case "test move" `Quick test_move ]);
    ]
