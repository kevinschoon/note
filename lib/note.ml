open Core
open Stdio

type t = Ezjsonm.t * Omd.t

let build ?(tags = []) ?(content = "") title =
  let frontmatter =
    Ezjsonm.dict
      [ ("title", Ezjsonm.string title); ("tags", Ezjsonm.strings tags) ]
  in
  (frontmatter, Omd.of_string content)

let get_title t =
  match Ezjsonm.find_opt (Ezjsonm.value (fst t)) [ "title" ] with
  | Some v -> Ezjsonm.get_string v
  | None -> ""

let get_tags t =
  match Ezjsonm.find_opt (Ezjsonm.value (fst t)) [ "tags" ] with
  | Some v -> Ezjsonm.get_strings v
  | None -> []

let to_json t =
  Ezjsonm.dict
    [
      ("frontmatter", Ezjsonm.value (fst t));
      ("content", Ezjsonm.string (Omd.to_text (snd t)));
    ]

let to_string t =
  let front_matter = Yaml.to_string_exn (Ezjsonm.value (fst t)) in
  String.concat ~sep:"\n" [ "---"; front_matter; "---"; Omd.to_text (snd t) ]

(*TODO: Change to Option*)
let of_string data =
  let indexes = String.substr_index_all ~may_overlap:true ~pattern:"---" data in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice data (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let frontmatter : Ezjsonm.t = match (Yaml.of_string_exn meta_str) with 
    | `O v -> `O v
    | `A v -> `A v
    | _ -> failwith "frontmatter is a partial fragment, should be either a dictionary or list"
    in
    let markdown : Omd.t =
      Omd.of_string
        (String.slice data (List.nth_exn indexes 1 + 3) (String.length data))
    in
    Some (frontmatter, markdown)
  else None

let of_string_exn data =
  let note = of_string data in
  match note with Some note -> note | None -> failwith "bad note content"

let read_note path =
  let data = In_channel.read_all path in
  of_string data

let read_note_exn path =
  let note = read_note path in
  match note with Some note -> note | None -> failwith "failed to read note"

let read_notes paths = List.map ~f:(fun path -> read_note_exn path) paths

let read_notes_with_paths paths =
  List.map ~f:(fun path -> (read_note_exn path, path)) paths

let filter (notes : t list) filters =
  if List.length filters = 0 then notes
    (* return everything if there are no filters *)
  else
    List.fold ~init:[]
      ~f:(fun accm note ->
        (* first look by name *)
        let matches =
          List.count
            ~f:(fun filter -> String.equal (get_title note) filter)
            filters
        in
        if matches > 0 then note :: accm
        else
          (* then compare each tag with each filter *)
          let matches =
            List.count
              ~f:(fun filter ->
                List.mem ~equal:String.equal (get_tags note) filter)
              filters
          in
          if matches > 0 then note :: accm else accm)
      notes

let filter_with_paths notes filters =
  if List.length filters = 0 then notes
    (* return everything if there are no filters *)
  else
    List.fold ~init:[]
      ~f:(fun accm (note, path) ->
        (* first look by name *)
        let matches =
          List.count
            ~f:(fun filter -> String.equal (get_title note) filter)
            filters
        in
        if matches > 0 then (note, path) :: accm
        else
          (* then compare each tag with each filter *)
          let matches =
            List.count
              ~f:(fun filter ->
                List.mem ~equal:String.equal (get_tags note) filter)
              filters
          in
          if matches > 0 then (note, path) :: accm else accm)
      notes
