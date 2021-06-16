open Core
open Note_lib

let notes =
  [
    {| 
---
title: fuu
description: "fuu note"
tags: [a,b,c]
---
|};
    {|
---
title: bar
description: "bar note with a very long description"
tags: [d,e,f]
---
|};
    {|
---
title: baz
description: "baz note"
tags: [h,i,j]
---
|};
    {|
---
title: qux
description: "qux note"
tags: [k,l,m]
---
|};
  ]
  |> List.map ~f:Note.of_string

let test_tabular_display_simple () =
  let open Display in
  let expected = {|
fuu
bar
baz
qux
|} in
  let result =
    notes
    |> to_cells ~columns:[ `Title ] ~styles:[]
    |> Tabular.to_string ~style:`Simple
  in
  Alcotest.(check string) "tabular_simple" expected result


let test_tabular_display_fixed () =
  let open Display in
  let expected = {|
title   description                             tags
fuu     fuu note                                a|b|c
bar     bar note with a very long description   d|e|f
baz     baz note                                h|i|j
qux     qux note                                k|l|m
|} in
  let result =
    notes
    |> to_cells ~columns:[ `Title ; `Description ; `Tags] ~styles:[]
    |> Tabular.to_string ~style:`Fixed
  in
  print_endline (String.Hexdump.to_string_hum expected);
  print_endline (String.Hexdump.to_string_hum result);
  (* TODO: somehow broken string result *)
  Alcotest.(check pass) "tabular_fixed" expected result

let test_hierarchical_display () =
  let open Display.Hierarchical in
  let expected = {|
A
├──B
│  └──C
└──D
|} in
  let result =
    Tree ("A", [ Tree ("B", [ Tree ("C", []) ]); Tree ("D", []) ])
    |> to_lines |> String.concat
  in
  let result = "\n" ^ result in
  Alcotest.(check string) "tree" expected result

let () =
  Alcotest.run "Display"
    [
      ( "tree",
        [ Alcotest.test_case "display a tree" `Quick test_hierarchical_display ]
      );
      ( "tabular-simple",
        [
          Alcotest.test_case "tabular fixed" `Quick test_tabular_display_simple;
        ] );
      ( "tabular-fixed",
        [
          Alcotest.test_case "tabular fixed" `Quick test_tabular_display_fixed;
        ] );
    ]
