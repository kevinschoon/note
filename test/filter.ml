open Note_lib

let test_filter_by_keys =
  let open Note.Filter in
  let note =
    Note.build ~tags:[ "fuu"; "bar" ] ~content:"" "A Very Important Note"
  in
  let result = find_many ~strategy: Keys ~args: ["fuu" ; "bar"; "baz"] [note] in
  assert (List.length result = 1)

let () =
  test_filter_by_keys;

