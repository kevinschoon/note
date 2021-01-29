open Core

let sync on_sync =
  match on_sync with
  | Some cmd -> Sys.command_exn cmd
  | None -> ()
