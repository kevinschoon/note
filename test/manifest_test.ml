open Core
open Note_lib

let test_recurse () =
  let manifest =
    Manifest.empty
    |> Manifest.insert ~path:"/" ~slug:"note-00000000-0" ~title:"a"
         ~description:"" ~tags:[]
    |> Manifest.insert ~path:"/a" ~slug:"note-00000000-1" ~title:"b"
         ~description:"" ~tags:[]
    |> Manifest.insert ~path:"/a/b" ~slug:"note-00000000-2" ~title:"c"
         ~description:"" ~tags:[]
    |> Manifest.insert ~path:"/a/b/c" ~slug:"note-00000000-3" ~title:"d"
         ~description:"" ~tags:[]
  in
  Alcotest.(check int) "n_results" 4 (List.length manifest.items)

let test_manifest () =
  let temp_db = Filename.temp_file "note-test" "" in
  Manifest.empty |> Manifest.save ~path:temp_db;
  let manifest =
    Manifest.load_or_init temp_db
    |> Manifest.insert ~path:"/" ~slug:"note-00000000-0.md" ~title:"fuu"
         ~description:"" ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu" in
  Alcotest.(check bool) "manifest loaded" (result |> Option.is_some) true;
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu" ~slug:"note-00000000-1.md" ~title:"bar"
         ~description:"" ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu/bar" in
  Alcotest.(check bool)
    "manifest /fuu/bar inserted" (result |> Option.is_some) true;
  let result_path = Option.value_exn result |> Manifest.to_path ~manifest in
  Alcotest.(check string) "result path" "/fuu/bar" result_path;
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu" ~slug:"note-00000000-2.md" ~title:"baz"
         ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu" in
  Alcotest.(check int) "n_results" 2 (List.length results);
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu/bar" ~slug:"note-00000000-3.md" ~title:"qux"
         ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu/bar" in
  Alcotest.(check int) "n_results" 1 (List.length results);
  Alcotest.(check int) "n_items" 4 (List.length manifest.items);
  print_endline (Manifest.to_string manifest);
  let manifest = manifest |> Manifest.remove ~path:"/fuu/bar/qux" in
  print_endline (Manifest.to_string manifest);
  Alcotest.(check int) "remove" 3 (List.length manifest.items)

let () =
  Alcotest.run "Config"
    [
      ("recurse", [ Alcotest.test_case "test recurse" `Quick test_recurse ]);
      ("load", [ Alcotest.test_case "test manifest" `Quick test_manifest ]);
    ]
