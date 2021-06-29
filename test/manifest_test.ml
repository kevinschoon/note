open Core
open Note_lib

let test_recurse () =
  let manifest =
    Manifest.make (Filename.temp_dir "note-test" "")
    |> Manifest.create ~path:"/a" ~description:"" ~tags:[]
    |> Manifest.create ~path:"/a/b" ~description:"" ~tags:[]
    |> Manifest.create ~path:"/a/b/c" ~description:"" ~tags:[]
    |> Manifest.create ~path:"/a/b/c/d" ~description:"" ~tags:[]
  in
  Alcotest.(check int) "n_results" 4 (List.length manifest.items)

let test_manifest () =
  let manifest = Manifest.make (Filename.temp_dir "note-test" "") in
  manifest |> Manifest.save;
  let manifest =
    Manifest.load_or_init manifest.state_dir
    |> Manifest.create ~path:"/fuu" ~description:"" ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu" in
  Alcotest.(check bool) "manifest loaded" (result |> Option.is_some) true;
  let manifest =
    manifest |> Manifest.create ~path:"/fuu/bar" ~description:"" ~tags:[]
  in
  let result = manifest |> Manifest.find ~path:"/fuu/bar" in
  Alcotest.(check bool)
    "manifest /fuu/bar inserted" (result |> Option.is_some) true;
  let result_path = (Option.value_exn result).path in
  Alcotest.(check string) "result path" "/fuu/bar" result_path;
  let manifest =
    manifest |> Manifest.create ~path:"/fuu/baz" ~description:"" ~tags:[]
  in
  let results = manifest |> Manifest.list ~path:"/fuu" in
  Alcotest.(check int) "n_results" 2 (List.length results);
  let manifest =
    manifest |> Manifest.create ~path:"/fuu/bar/qux" ~description:"" ~tags:[]
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
    Manifest.make (Filename.temp_dir "note-test" "")
    |> Manifest.create ~path:"/a" ~description:"" ~tags:[]
    |> Manifest.create ~path:"/a/b" ~description:"" ~tags:[]
  in
  Alcotest.(check int) "two entries" 2 (List.length manifest.items);
  let manifest =
    manifest |> Manifest.update ~path:"/a/b" ~description:"" ~tags:[ "a"; "b" ]
  in
  let result = Option.value_exn (manifest |> Manifest.find ~path:"/a/b") in
  Alcotest.(check string) "updated" "a" (List.nth_exn result.tags 0);
  Alcotest.(check string) "updated" "b" (List.nth_exn result.tags 1);

  Alcotest.(check int) "two entries" 2 (List.length manifest.items)

let () =
  Alcotest.run "Manifest"
    [
      ( "successive inserts",
        [
          Alcotest.test_case "insert several items into the manifest" `Quick
            test_recurse;
        ] );
      ( "manifest",
        [ Alcotest.test_case "test basic manifest" `Quick test_manifest ] );
      ( "update",
        [ Alcotest.test_case "test manifest update / move" `Quick test_update ]
      );
    ]
