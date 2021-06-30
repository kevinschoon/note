open Core
open Note_lib

let test_recurse () =
  let manifest =
    Manifest.make (Filename.temp_dir "note-test" "")
    |> Manifest.create ~path:"/"
    |> Manifest.create ~path:"/a"
    |> Manifest.create ~path:"/a/b"
    |> Manifest.create ~path:"/a/b/c"
    |> Manifest.create ~path:"/a/b/c/d"
  in
  Alcotest.(check int) "n_results" 5 (List.length manifest.items) ;
  Alcotest.(check int) "n_results" 1 (List.length (manifest |> Manifest.list ~path:"/"));
  Alcotest.(check int) "n_results" 1 (List.length (manifest |> Manifest.list ~path:"/a"));
  Alcotest.(check int) "n_results" 1 (List.length (manifest |> Manifest.list ~path:"/a/b"));
  Alcotest.(check int) "n_results" 1 (List.length (manifest |> Manifest.list ~path:"/a/b/c"));
  Alcotest.(check int) "n_results" 0 (List.length (manifest |> Manifest.list ~path:"/a/b/c/d"))

let test_manifest () =
  let manifest = Manifest.make (Filename.temp_dir "note-test" "") in
  manifest |> Manifest.save;
  let manifest =
    Manifest.load_or_init manifest.state_dir |> Manifest.create ~path:"/fuu"
  in
  let result = manifest |> Manifest.find ~path:"/fuu" in
  Alcotest.(check bool) "manifest loaded" (result |> Option.is_some) true;
  let manifest = manifest |> Manifest.create ~path:"/fuu/bar" in
  let result = manifest |> Manifest.find ~path:"/fuu/bar" in
  Alcotest.(check bool)
    "manifest /fuu/bar inserted" (result |> Option.is_some) true;
  let result_path = (Option.value_exn result).path in
  Alcotest.(check string) "result path" "/fuu/bar" result_path;
  let manifest = manifest |> Manifest.create ~path:"/fuu/baz" in
  let results = manifest |> Manifest.list ~path:"/fuu" in
  Alcotest.(check int) "n_results" 2 (List.length results);
  let manifest = manifest |> Manifest.create ~path:"/fuu/bar/qux" in
  let results = manifest |> Manifest.list ~path:"/fuu/bar" in
  Alcotest.(check int) "n_results" 1 (List.length results);
  Alcotest.(check int) "n_items" 4 (List.length manifest.items);
  print_endline (Manifest.to_string manifest);
  let manifest = manifest |> Manifest.remove ~path:"/fuu/bar/qux" in
  print_endline (Manifest.to_string manifest);
  Alcotest.(check int) "remove" 3 (List.length manifest.items)

let test_move () =
  let manifest =
    Manifest.make (Filename.temp_dir "note-test" "")
    |> Manifest.create ~path:"/a"
    |> Manifest.create ~path:"/a/b"
  in
  Alcotest.(check int) "two entries" 2 (List.length manifest.items);
  Alcotest.(check bool)
    "exists" true
    (Option.is_some (manifest |> Manifest.find ~path:"/a/b"));
  let manifest = manifest |> Manifest.move ~source:"/a/b" ~dest:"/b" in
  Alcotest.(check int) "two entries" 2 (List.length manifest.items);
  Alcotest.(check bool)
    "exists" true
    (Option.is_some (manifest |> Manifest.find ~path:"/b"));
  Alcotest.(check bool)
    "exists" false
    (Option.is_some (manifest |> Manifest.find ~path:"/a/b"))

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
      ("update", [ Alcotest.test_case "test manifest move" `Quick test_move ]);
    ]
