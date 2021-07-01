open Core

let prompt ~callback message =
  print_endline message;
  print_endline "Type YES to continue";
  let input = In_channel.(input_line stdin) in
  match input with
  | Some value -> (
      match value with "YES" -> callback () | _ -> print_endline "aborted")
  | None -> ()
