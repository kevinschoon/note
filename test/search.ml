open Note_lib

let make_notes =
  let state_dir = Core.Filename.temp_dir "note-test" "" in
  let note_1 =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" ~title:"A Very Important Note"
      (Slug.next state_dir)
  in
  let note_2 =
    Note.build ~tags:[ "fuu"; "bar"; "baz" ] ~content:""
      ~title:"Another Very Important Note" (Slug.next state_dir)
  in
  let note_3 =
    Note.build ~tags:[ "fuu"; "bar"; "baz" ] ~content:""
      ~title:"fuu" (Slug.next state_dir)
  in
  [note_1 ; note_2 ; note_3]


let test_filter_by_keys =
  let notes = make_notes in
  let result =
    Note.Search.find_many ~args:[ "fuu"; "bar"; "baz" ]
    notes
  in
  assert (List.length result = 3)

let test_filter_by_title_find_one =
  let notes = make_notes in
  let result =
    Note.Search.find_one ~args:[ "fuu" ]
    notes
  in
  assert (Option.is_some result) ;
  let note = Option.get result in
  (* title should take priority *)
  assert ((Note.get_title note) = "fuu")

let () = 
  test_filter_by_keys;
  test_filter_by_title_find_one;
