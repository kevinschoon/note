open Core

let pattern = Re.Pcre.regexp {|(.*)?note-(\d{8})-(\d{1,})(.md)?|}

type t = { path : string; date : Date.t; index : int }

let to_string slug = slug.path

let of_string ?(basepath = None) path =
  let result = Re.all pattern path |> List.hd_exn in
  let items = Re.Group.all result |> Array.to_list in
  let date = Date.parse ~fmt:"%Y%m%d" (List.nth_exn items 2) in
  let index = int_of_string (List.nth_exn items 3) in
  let path =
    match basepath with
    | Some basepath -> Filename.concat basepath path
    | None -> path
  in
  let path =
    match Filename.check_suffix path "md" with
    | true -> path
    | false -> String.concat [ path; ".md" ]
  in
  { path; date; index }

let shortname t =
  let date_str = Date.format t.date "%Y%m%d" in
  sprintf "note-%s-%d" date_str t.index

let append ~path t =
  let path = Filename.concat path t.path in
  { t with path }

let compare s1 s2 = String.compare s1.path s2.path

let is_note path =
  Filename.basename path |> String.is_substring ~substring:"note-"

let load state_dir =
  state_dir |> Sys.ls_dir |> List.filter ~f:is_note
  |> List.map ~f:(Filename.concat state_dir)
  |> List.map ~f:of_string

let next ?(last = None) state_dir =
  let today = Time.now () |> Time.to_date ~zone:Time.Zone.utc in
  let today_str = Date.format today "%Y%m%d" in
  match last with
  | Some last ->
      let tomorrow = Date.add_days today 1 in
      if Date.between ~low:today ~high:tomorrow last.date then
        let index = last.index + 1 in
        let path =
          Filename.concat state_dir (sprintf "note-%s-%d.md" today_str index)
        in
        { path; date = today; index }
      else
        let index = 0 in
        let path =
          Filename.concat state_dir (sprintf "note-%s-%d.md" today_str index)
        in
        { path; date = today; index }
  | None ->
      let index = 0 in
      let path =
        Filename.concat state_dir (Core.sprintf "note-%s-%d.md" today_str index)
      in
      { path; date = today; index }
