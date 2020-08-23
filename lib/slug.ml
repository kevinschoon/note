open Core

type t = Date.t * int

let to_string slug =
  let time, index = slug in
  let date_str = Date.format time "%Y%m%d" in
  sprintf "note-%s-%d.md" date_str index

let of_string slug_str =
  (* note-20010103-0.md *)
  let slug = Filename.chop_extension (Filename.basename slug_str) in
  let split = String.split ~on:'-' slug in
  (* TODO: add proper error handling *)
  ( Date.parse ~fmt:"%Y%m%d" (List.nth_exn split 1),
    int_of_string (List.nth_exn split 2) )

let load path =
  let file_names = Sys.ls_dir path in
  List.filter_map
    ~f:(fun name ->
      (* ignore any files that do not match the substring note- *)
      match String.substr_index ~pattern:"note-" name with
      | Some _ -> Some (of_string name)
      | None -> None)
    file_names

let next slugs =
  (* find all slugs for today (00:00:00 -> 23:59:59) *)
  let now = Time.now () in
  let today = Time.to_date ~zone:Time.Zone.utc now in
  let tomorrow = Date.add_days today 1 in
  let filtered =
    List.filter
      ~f:(fun (time, _) -> Date.between ~low:today ~high:tomorrow time)
      slugs
  in
  let next_int =
    List.fold ~init:(-1)
      ~f:(fun accm (_, i) -> if i > accm then i else accm)
      filtered
    + 1
  in
  (today, next_int)
