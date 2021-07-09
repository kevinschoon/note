open Core
open Note_lib.Display

let noop s = s

let test_tabular_display_fixed () =
  let rows =
    [
      [ ("AAA", noop); ("BBBB", noop); ("CCCC", noop) ];
      [ ("aaaaaaa", noop); ("b", noop); ("cccccccc", noop) ];
      [ ("aa", noop); ("bbb", noop); ("c", noop) ];
    ]
  in
  let expected = {|
AAA     BBBB CCCC     
aaaaaaa b    cccccccc 
aa      bbb  c        |} in
  let result = "\n" ^ (rows |> Tabular.fixed |> String.concat ~sep:"\n") in
  print_endline "EXPECTED:" ;
  print_endline (String.Hexdump.to_string_hum expected);
  print_endline "RESULT:" ;
  print_endline (String.Hexdump.to_string_hum result);
  Alcotest.(check string) "tabular_fixed" expected result

let test_tabular_display_wide () =
  let rows =
    [
      [ ("AAA", noop); ("BBBB", noop); ("CCCC", noop) ];
      [ ("aaaaaaa", noop); ("b", noop); ("cccccccc", noop) ];
      [ ("aa", noop); ("bbb", noop); ("c", noop) ];
    ]
  in
  let expected = {|
AAA             BBBB CCCC     
aaaaaaa         b    cccccccc 
aa              bbb  c        |} in
  let result = "\n" ^ (rows |> Tabular.wide ~width:30 |> String.concat ~sep:"\n") in
  print_endline "EXPECTED:" ;
  print_endline (String.Hexdump.to_string_hum expected);
  print_endline "RESULT:" ;
  print_endline (String.Hexdump.to_string_hum result);
  Alcotest.(check string) "tabular_fixed" expected result

let test_hierarchical_display () =
  let open Hierarchical in
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
      ( "tabular-fixed",
        [ Alcotest.test_case "tabular fixed" `Quick test_tabular_display_fixed ]
      );
      ( "tabular-wide",
        [ Alcotest.test_case "tabular wide" `Quick test_tabular_display_wide ]
      );
    ]
