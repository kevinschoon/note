open Core
open Note_lib

let test_slug_of_string () =
  let input = "/fuu/bar/note-19700101-0.md" in
  let slug = input |> Slug.of_string in
  Alcotest.(check string) "conversion" input (slug |> Slug.to_string);
  let input = "note-19700101-0.md" in
  let slug = input |> Slug.of_string in
  Alcotest.(check string) "bare" input (slug |> Slug.to_string)

let test_slug_shortname () =
  let s1 = Slug.of_string "fuu/note-19700101-0.md" in
  Alcotest.(check string) "short name" "note-19700101-0" (Slug.shortname s1)

let test_slug_comparable () =
  let s1, s2 =
    ( Slug.of_string "fuu/note-19700101-0.md",
      Slug.of_string "fuu/note-19700101-0.md" )
  in
  Alcotest.(check bool) "identical paths" true (Slug.compare s1 s2 = 0)

let test_slug_path () =
  let date_string = Time_unix.format (Time.now ()) "%Y%m%d" ~zone:Time.Zone.utc in
  let state_dir = Filename_unix.temp_dir "note-test" "" in
  let slug = Slug.next state_dir in
  let expected =
    Filename.concat state_dir (sprintf "note-%s-0.md" date_string)
  in
  Alcotest.(check string) "path" expected slug.path

let test_slug_increment () =
  let date_string = Time_unix.format (Time.now ()) "%Y%m%d" ~zone:Time.Zone.utc in
  let slug = Slug.next "/fuu/bar" in
  let expected = sprintf "/fuu/bar/note-%s-%d.md" date_string 0 in
  Alcotest.(check string) "check path" expected (slug |> Slug.to_string);
  let slug = Slug.next ~last:(Some slug) "/fuu/bar" in
  let expected = sprintf "/fuu/bar/note-%s-%d.md" date_string 1 in
  Alcotest.(check string) "check path" expected (slug |> Slug.to_string)

let () =
  Alcotest.run "Slug"
    [
      ( "slug-of-string",
        [ Alcotest.test_case "ofstring" `Quick test_slug_of_string ] );
      ( "slug-comparable",
        [ Alcotest.test_case "compare" `Quick test_slug_comparable ] );
      ( "slug-shortname",
        [ Alcotest.test_case "shortname" `Quick test_slug_shortname ] );
      ("path", [ Alcotest.test_case "path" `Quick test_slug_path ]);
      ( "slug-increment",
        [ Alcotest.test_case "increment" `Quick test_slug_increment ] );
    ]
