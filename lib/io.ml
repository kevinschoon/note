open Core

let create ~callback ~content dest =
  Out_channel.write_all ~data:content dest;
  match callback with Some cmd -> Sys.command_exn cmd | None -> ()

let create_on_change ~callback ~editor ~content dest =
  let tmp_file = Filename.temp_file "note" ".md" in
  Out_channel.write_all ~data:content tmp_file;
  let command = sprintf "%s %s" editor tmp_file in
  Sys.command_exn command;
  let new_content = In_channel.read_all tmp_file in
  if not (String.equal content new_content) then
    Out_channel.write_all ~data:new_content dest;
  match callback with Some cmd -> Sys.command_exn cmd | None -> ()

let edit ~callback ~editor path =
  let orig_content = In_channel.read_all path in
  let command = sprintf "%s %s" editor path in
  Sys.command_exn command;
  let new_content = In_channel.read_all path in
  if not (String.equal orig_content new_content) then
    match callback with Some cmd -> Sys.command_exn cmd | None -> ()

let delete ~callback ~title path =
  let colorize_title title = 
  let open ANSITerminal in
  (sprintf [ ANSITerminal.Bold ] "%s" title) in
  print_endline (sprintf "Are you sure you want to delete the following note: %s?" (colorize_title title)) ;
  print_endline "Type YES to continue" ;
  let input = In_channel.(input_line stdin) in 
  match input with
  | Some value -> 
    if String.equal value "YES" then
    (Unix.remove path ;
    (match callback with Some cmd -> Sys.command_exn cmd | None -> ()))
    else
    print_endline "No changes made"
  | None -> ()


let read path =
  In_channel.read_all path
