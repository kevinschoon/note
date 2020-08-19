open Core;;

let create_edit_write init dest =
    let tmp_file = Filename.temp_file "note" ".md" in
    (Out_channel.write_all ~data: init tmp_file) ;
    let editor = Sys.getenv_exn "EDITOR" in
        Sys.command_exn (sprintf "%s %s" editor tmp_file) ;
    let modified = In_channel.read_all tmp_file in 
        (Out_channel.write_all ~data: modified dest);
