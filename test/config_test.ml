open Core
open Note_lib

let test_configuration () =
  let config_path = Filename_unix.temp_file "note-test" "" in
  let _ = config_path |> Config.load in
  Alcotest.(check bool) "config loaded" true true

let () =
  Alcotest.run "Config"
    [
      ( "load",
        [ Alcotest.test_case "test configuration" `Quick test_configuration ] );
    ]
