open Core
open Note_lib

let test_configuration () =
  let config_path = Filename.temp_file "note-test" "" in
  let cfg = config_path |> Config.load in
  Alcotest.(check bool) "config loaded" true (cfg.context |> Note.Term.is_empty)

let () =
  Alcotest.run "Config"
    [
      ( "load",
        [ Alcotest.test_case "test configuration" `Quick test_configuration ] );
    ]
