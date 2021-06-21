open Core
open Note_lib

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
  print_endline (Manifest.to_string manifest);
  let result = manifest |> Manifest.find ~path:"/fuu/bar" in
  Alcotest.(check bool) "manifest loaded" (result |> Option.is_some) true;
  let result_path = Option.value_exn result |> Manifest.to_path ~manifest in
  Alcotest.(check string) "result path" "/fuu/bar" result_path;
  let manifest =
    manifest
    |> Manifest.insert ~path:"/fuu" ~slug:"note-00000000-2.md" ~title:"baz"
         ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu" in
  Alcotest.(check int) "n_results" 2 (List.length results)

let () =
  Alcotest.run "Config"
    [
      ("load", [ Alcotest.test_case "test manifest" `Quick test_manifest ]);
    ]
