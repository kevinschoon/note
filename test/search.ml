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
    Note.build ~tags:[ "fuu"; "bar"; "baz" ] ~content:"" ~title:"fuu"
      (Slug.next state_dir)
  in
  [ note_1; note_2; note_3 ]

let test_filter_by_keys =
  let notes = make_notes in
  let result =
    Note.find_many
      ~term:
        {
          titles = [ Re.Str.regexp "A Very Important Note" ];
          tags = [];
          operator = Note.Or;
        }
      notes
  in
  assert (List.length result = 1);
  let result =
    Note.find_many
      ~term:
        {
          titles = [ Re.Str.regexp "A Very Important Note" ];
          tags = [];
          operator = Note.Or;
        }
      notes
  in
  assert (List.length result = 1);
  let result =
    Note.find_many
      ~term:
        {
          titles = [];
          tags =
            [ Re.Str.regexp "fuu"; Re.Str.regexp "bar"; Re.Str.regexp "baz" ];
          operator = Note.And;
        }
      notes
  in
  assert (List.length result = 2)

let test_filter_by_title_find_one =
  let notes = make_notes in
  let result =
    Note.find_one
      ~term:{ titles = [Re.Str.regexp "^A.*"]; tags = []; operator = Note.Or }
      notes
  in
  assert (Option.is_some result);
  let note = Option.get result in
  assert (Note.get_title note = "A Very Important Note")

let () =
  test_filter_by_keys;
  test_filter_by_title_find_one
