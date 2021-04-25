open Core

type t = {
  frontmatter : Ezjsonm.t;
  content : string;
  markdown : Omd.doc;
  slug : Slug.t;
  parent : Slug.t option;
}

let build ?(description = "") ?(tags = []) ?(content = "") ~title slug =
  let frontmatter =
    Ezjsonm.dict
      [
        ("title", Ezjsonm.string title);
        ("description", Ezjsonm.string description);
        ("tags", Ezjsonm.strings tags);
      ]
  in
  let markdown = Omd.of_string content in
  { frontmatter; content; markdown; slug; parent = None }

let get_title t =
  (* if title is specified use that, otherwise fall back to slug *)
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "title" ] with
  | Some title -> Ezjsonm.get_string title
  | None -> Slug.to_string t.slug

let get_description t =
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "description" ] with
  | Some description -> Ezjsonm.get_string description
  | None -> ""

let get_tags t =
  match Ezjsonm.find_opt (Ezjsonm.value t.frontmatter) [ "tags" ] with
  | Some tags -> Ezjsonm.get_strings tags
  | None -> []

let get_path t = Slug.get_path t.slug

let rec extract_structured_data (accm : Ezjsonm.value list) (doc : Omd.doc) :
    Ezjsonm.value list =
  match doc with
  | [] -> accm
  | hd :: tl -> (
      match hd.bl_desc with
      | Code_block (kind, content) -> (
          match kind with
          | "json" ->
              let accm = accm @ [ Ezjsonm.from_string content ] in
              extract_structured_data accm tl
          | "yaml" ->
              let accm = accm @ [ Ezjsonm.wrap (Yaml.of_string_exn content) ] in
              extract_structured_data accm tl
          | _ -> extract_structured_data accm tl)
      | _ -> extract_structured_data accm tl)

let get_data t =
  let data = extract_structured_data [] t.markdown in
  Ezjsonm.list (fun value -> value) data

let to_json t =
  Ezjsonm.dict
    [
      ("frontmatter", Ezjsonm.value t.frontmatter);
      ("content", Ezjsonm.string t.content);
      ("data", get_data t);
    ]

let to_string t =
  let yaml = Yaml.to_string_exn (Ezjsonm.value t.frontmatter) in
  "\n---" ^ yaml ^ "\n---\n" ^ t.content

let of_string ~content slug =
  let indexes =
    String.substr_index_all ~may_overlap:true ~pattern:"---" content
  in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice content (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
    in
    let frontmatter : Ezjsonm.t =
      match Yaml.of_string_exn meta_str with
      | `O v -> `O v
      | `A v -> `A v
      | _ ->
          failwith
            "frontmatter is a partial fragment, should be either a dictionary \
             or list"
    in
    let markdown : Omd.doc =
      Omd.of_string
        (String.slice content
           (List.nth_exn indexes 1 + 3)
           (String.length content))
    in
    { frontmatter; content; markdown; slug; parent = None }
  else
    let frontmatter = Ezjsonm.dict [] in
    let markdown = Omd.of_string content in
    { frontmatter; content; markdown; slug; parent = None }

module Util = struct
  let split_words str =
    List.filter_map
      ~f:(fun x ->
        match String.strip ~drop:(fun x -> Char.equal x ' ') x with
        | "" -> None
        | _ -> Some x)
      (String.split ~on:' ' str)

  let rec to_words (accm : string list) (doc : Omd.doc) : string list =
    let split_words inline =
      match inline with Omd.Text text -> String.split ~on:' ' text | _ -> []
    in
    match doc with
    | [] -> accm
    | hd :: tl -> (
        (* TODO: account for headings, lists, etc *)
        match hd.bl_desc with
        | Paragraph inline ->
            let accm = accm @ split_words inline.il_desc in
            to_words accm tl
        | _ -> to_words accm tl)
end

module Encoding = struct
  let to_string ~style t =
    match style with
    | `Raw -> In_channel.read_all (get_path t)
    | `Json -> Ezjsonm.to_string (to_json t)
    | `Yaml -> Yaml.to_string_exn (to_json t)
    | `Html -> Omd.to_html t.markdown
end

module Search = struct
  open Re.Str

  let dump_results results =
    List.iter
      ~f:(fun result ->
        print_endline (sprintf "%s - %d" (get_title (snd result)) (fst result)))
      results

  let title expr note =
    let title_string = get_title note in
    string_match expr title_string 0

  let tags expr note =
    let tags = get_tags note in
    List.count ~f:(fun tag -> string_match expr tag 0) tags > 0

  let content expr note =
    let words = Util.to_words [] note.markdown in
    List.count ~f:(fun word -> string_match expr word 0) words > 0

  let match_and_rank ~args notes =
    let expressions = List.map ~f:regexp args in
    let matches =
      List.fold ~init:[]
        ~f:(fun accm note ->
          let has_title =
            List.count ~f:(fun expr -> title expr note) expressions > 0
          in
          let has_tag =
            List.count ~f:(fun expr -> tags expr note) expressions > 0
          in
          let has_content =
            List.count ~f:(fun expr -> content expr note) expressions > 0
          in
          match (has_title, has_tag, has_content) with
          | true, _, _ -> List.append accm [ (20, note) ]
          | _, true, _ -> List.append accm [ (10, note) ]
          | _, _, true -> List.append accm [ (5, note) ]
          | false, false, false -> accm)
        notes
    in
    List.rev (List.sort ~compare:(fun n1 n2 -> fst n1 - fst n2) matches)

  let find_one ~args notes =
    let results = match_and_rank ~args notes in
    let results = List.map ~f:snd results in
    if List.length results = 0 then None else Some (List.hd_exn results)

  let find_many ~args notes =
    if List.length args = 0 then notes
    else
      let results = match_and_rank ~args notes in
      List.map ~f:snd results
end

open ANSITerminal

let paint_tag (styles : Config.StylePair.t list) text : string =
  match List.find ~f:(fun entry -> String.equal entry.pattern text) styles with
  | Some entry -> sprintf entry.styles "%s" text
  | None -> sprintf [ Foreground Default ] "%s" text

let to_cells ~columns ~styles notes =
  let header =
    List.map
      ~f:(fun column ->
        let text_value = Config.Column.to_string column in
        let text_length = String.length text_value in
        let text_value = sprintf [ Bold; Underlined ] "%s" text_value in
        (text_value, text_length, 1))
      columns
  in
  let note_cells =
    let default_padding = 1 in
    List.fold ~init:[]
      ~f:(fun accm note ->
        accm
        @ [
            List.map
              ~f:(fun column ->
                match column with
                | `Title ->
                    let text_value = get_title note in
                    (text_value, String.length text_value, default_padding)
                | `Description ->
                    let text_value = get_description note in
                    (text_value, String.length text_value, default_padding)
                | `Tags ->
                    let text_value = String.concat ~sep:"|" (get_tags note) in
                    let text_length = String.length text_value in
                    let tags = get_tags note in
                    let tags =
                      List.map ~f:(fun tag -> paint_tag styles tag) tags
                    in
                    let text_value = String.concat ~sep:"|" tags in
                    (text_value, text_length, default_padding)
                | `WordCount ->
                    let text_value =
                      Core.sprintf "%d"
                        (List.length (Util.to_words [] note.markdown))
                    in
                    (text_value, String.length text_value, default_padding)
                | `Slug ->
                    let text_value = Slug.to_string note.slug in
                    (text_value, String.length text_value, default_padding))
              columns;
          ])
      notes
  in
  [ header ] @ note_cells
