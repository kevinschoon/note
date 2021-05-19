open Core
open Note_lib

let test_slug_shortname () =
  let s1 = Slug.of_path "fuu/note-19700101-0.md" in
  Alcotest.(check string) "short name" "note-19700101-0" (Slug.shortname s1)

let test_slug_comparable () =
  let s1, s2 =
    ( Slug.of_path "fuu/note-19700101-0.md",
      Slug.of_path "fuu/note-19700101-0.md" )
  in
  Alcotest.(check bool) "identical paths" true (Slug.compare s1 s2 = 0)

let test_slug_path () =
  let date_string = Time.format (Time.now ()) "%Y%m%d" ~zone:Time.Zone.utc in
  let state_dir = Filename.temp_dir "note-test" "" in
  let slug = Slug.next state_dir in
  let expected =
    Filename.concat state_dir (sprintf "note-%s-0.md" date_string) 
  in
  Alcotest.(check string) "path" expected slug.path

let test_slug_increment () =
  let state_dir = Filename.temp_dir "note-test" "" in
  let date_string = Time.format (Time.now ()) "%Y%m%d" ~zone:Time.Zone.utc in
  for i = 0 to 5 do
    let filename =
      Filename.concat state_dir (sprintf "note-%s-%d.md" date_string i)
    in
    Out_channel.write_all filename ~data:""
  done;
  let slug = Slug.next state_dir in
  Alcotest.(check int) "index" 6 slug.index

let () =
  Alcotest.run "Slug"
    [
      ( "slug-comparable",
        [ Alcotest.test_case "compare" `Quick test_slug_comparable ] );
      ( "slug-shortname",
        [ Alcotest.test_case "shortname" `Quick test_slug_shortname ] );
      ("path", [ Alcotest.test_case "path" `Quick test_slug_path ]);
      ( "slug-increment",
        [ Alcotest.test_case "increment" `Quick test_slug_increment ] );
    ]
