open Note_lib

let test_filter_by_keys =
  let state_dir = Core.Filename.temp_dir "note-test" "" in
  let slug = Slug.next state_dir in
  let note =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" ~title:"A Very Important Note" slug
  in
  let result = Note.Filter.find_many ~strategy: Keys ~args: ["fuu" ; "bar"; "baz"] [note] in
  assert (List.length result = 1)

let () =
  test_filter_by_keys;
