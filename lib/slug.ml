open Core

type t = { path : string; date : Date.t; index : int }

let get_path t = t.path

let to_string t =
  let date_str = Date.format t.date "%Y%m%d" in
  sprintf "%s-%d" date_str t.index

let of_path path =
  (* note-20010103-0.md *)
  if is_some (String.substr_index ~pattern:"note-" path) then
    let slug = Filename.chop_extension (Filename.basename path) in
    let split = String.split ~on:'-' slug in
    (* TODO: add proper error handling *)
    let date = Date.parse ~fmt:"%Y%m%d" (List.nth_exn split 1) in
    let index = int_of_string (List.nth_exn split 2) in
    Some { path; date; index }
  else None

let load state_dir =
  List.filter_map
    ~f:(fun path -> of_path (Filename.concat state_dir path))
    (Sys.ls_dir state_dir)

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
    Filename.concat state_dir (Core.sprintf "%s-%d.md" date_str next_int)
  in
  { path; date = today; index = next_int }
