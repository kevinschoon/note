let sync on_sync =
  match on_sync with
  | Some cmd -> Sys_unix.command_exn cmd
  | None -> ()
