open Core

type t = { path : string; date : Date.t; index : int }

let shortname t =
  let date_str = Date.format t.date "%Y%m%d" in
  sprintf "note-%s-%d" date_str t.index

let compare s1 s2 = String.compare s1.path s2.path

let is_note path =
  Filename.basename path |> String.is_substring ~substring:"note-"

let of_path path =
  let slug = Filename.chop_extension (Filename.basename path) in
  let split = String.split ~on:'-' slug in
  let date = Date.parse ~fmt:"%Y%m%d" (List.nth_exn split 1) in
  let index = int_of_string (List.nth_exn split 2) in
  { path; date; index }

let load state_dir =
  state_dir |> Sys.ls_dir |> List.filter ~f:is_note
  |> List.map ~f:(Filename.concat state_dir)
  |> List.map ~f:of_path

let next state_dir =
  let slugs = load state_dir in
  (* find all slugs for today (00:00:00 -> 23:59:59) *)
  let now = Time.now () in
  let today = Time.to_date ~zone:Time.Zone.utc now in
  let tomorrow = Date.add_days today 1 in
  let filtered =
    List.filter
      ~f:(fun slug -> Date.between ~low:today ~high:tomorrow slug.date)
      slugs
  in
  let next_int =
    List.fold ~init:(-1)
      ~f:(fun accm slug -> if slug.index > accm then slug.index else accm)
      filtered
    + 1
  in
  let date_str = Date.format today "%Y%m%d" in
  let path =
    Filename.concat state_dir (Core.sprintf "note-%s-%d.md" date_str next_int)
  in
  { path; date = today; index = next_int }
