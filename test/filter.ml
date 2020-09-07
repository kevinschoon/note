open Note_lib

let test_filter_by_keys =
  let open Note.Filter in
  let note =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" "A Very Important Note"
  in
  let result = find_many (of_strings Keys [ "fuu"; "bar"; "baz" ]) [ note ] in
  assert (List.length result = 1)

let test_filter_by_subset =
  let open Note.Filter in
  let note =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" "A Very Important Note"
  in
  let result =
    find_many (of_strings Subset [ "[\"fuu\", \"bar\"]" ]) [ note ]
  in
  assert (List.length result = 1)

let test_filter_by_path =
  let open Note.Filter in
  let note =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" "A Very Important Note"
  in
  let result =
    find_many (of_strings Path [ ".frontmatter.tags" ; "[\"fuu\", \"bar\"]" ]) [ note ]
  in
  assert (List.length result = 1)



let () =
  test_filter_by_keys;
  test_filter_by_subset;
  test_filter_by_path;
