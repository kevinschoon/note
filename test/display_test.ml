open Note_lib

let test_tree () =
  let open Display.Tree in
  let expected = {|
A
├──B
│  └──C
└──D
|} in
  let result =
    Tree ("A", [ Tree ("B", [ Tree ("C", []) ]); Tree ("D", []) ])
    |> to_lines |> String.concat ""
  in
  let result = "\n" ^ result in
  Alcotest.(check string) "tree" expected result

let () =
  Alcotest.run "Display"
    [ ("tree", [ Alcotest.test_case "display a tree" `Quick test_tree ]) ]
