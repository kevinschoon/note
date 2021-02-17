open Core

type t = { frontmatter : Ezjsonm.t option; markdown : Omd.t; slug : Slug.t }

let build ?(tags = []) ?(content = "") ~title slug =
  let frontmatter =
    Some
      (Ezjsonm.dict
         [ ("title", Ezjsonm.string title); ("tags", Ezjsonm.strings tags) ])
  in
  let markdown = Omd.of_string content in
  { frontmatter; markdown; slug }

let get_title t =
  let title =
    match t.frontmatter with
    | Some fm -> (
        match Ezjsonm.find_opt (Ezjsonm.value fm) [ "title" ] with
        | Some v -> Some (Ezjsonm.get_string v)
        | None -> None )
    | None -> None
  in
  match title with
  | Some title -> title
  (* Since we couldn't determine the title from frontmatter now we will
     infer the title by looking at the markdown *)
  | None -> (
      let title =
        List.find
          ~f:(fun entry ->
            match entry with
            | Omd.H1 _ | Omd.H2 _ | Omd.H3 _ | Omd.H4 _ | Omd.H5 _ | Omd.H6 _ ->
                true
            | _ -> false)
          t.markdown
      in
      match title with Some e -> Omd_backend.text_of_md [ e ] | None -> "???" )

let get_description t =
  let description =
    match t.frontmatter with
    | Some fm -> (
        match Ezjsonm.find_opt (Ezjsonm.value fm) [ "description" ] with
        | Some v -> Some (Ezjsonm.get_string v)
        | None -> None )
    | None -> None
  in
  match description with Some description -> description | None -> ""

let get_tags t =
  match t.frontmatter with
  | Some fm -> (
      match Ezjsonm.find_opt (Ezjsonm.value fm) [ "tags" ] with
      | Some v -> Ezjsonm.get_strings v
      | None -> [] )
  | None -> []

let get_path t = Slug.get_path t.slug

let get_data t =
  let data =
    List.filter_map
      ~f:(fun entry ->
        match entry with
        | Code (language, data) | Code_block (language, data) -> (
            match language with
            | "JSON" | "Json" | "json" ->
                Some (Ezjsonm.value (Ezjsonm.from_string data))
            | "YAML" | "Yaml" | "yaml" -> Some (Yaml.of_string_exn data)
            | _ -> None )
        | _ -> None)
      t.markdown
  in
  Ezjsonm.list (fun value -> value) data

let to_json t =
  let frontmatter =
    match t.frontmatter with
    | Some fm -> Ezjsonm.value fm
    | None -> Ezjsonm.unit ()
  in
  Ezjsonm.dict
    [
      ("frontmatter", frontmatter);
      ("content", Ezjsonm.string (Omd.to_text t.markdown));
      ("data", get_data t);
    ]

let to_string t =
  match t.frontmatter with
  | Some fm ->
      let front_matter = Yaml.to_string_exn (Ezjsonm.value fm) in
      String.concat ~sep:"\n"
        [ "---"; front_matter; "---"; Omd.to_text t.markdown ]
  | None -> Omd.to_text t.markdown

let of_string ~data slug =
  let indexes = String.substr_index_all ~may_overlap:true ~pattern:"---" data in
  if List.length indexes >= 2 then
    let meta_str =
      String.slice data (List.nth_exn indexes 0 + 3) (List.nth_exn indexes 1)
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
    let markdown : Omd.t =
      Omd.of_string
        (String.slice data (List.nth_exn indexes 1 + 3) (String.length data))
    in
    let frontmatter = Some frontmatter in
    { frontmatter; markdown; slug }
  else
    let frontmatter = None in
    let markdown = Omd.of_string data in
    { frontmatter; markdown; slug }

module Util = struct
  let split_words str =
    List.filter_map
      ~f:(fun x ->
        match String.strip ~drop:(fun x -> Char.equal x ' ') x with
        | "" -> None
        | _ -> Some x)
      (String.split ~on:' ' str)

  let rec to_words markdown =
    match markdown with
    | [] -> []
    | hd :: tl ->
        ( match hd with
        | Omd.Text s -> split_words s
        | Omd.H1 v
        | Omd.H2 v
        | Omd.H3 v
        | Omd.H4 v
        | Omd.H5 v
        | Omd.H6 v
        | Omd.Blockquote v
        | Omd.Bold v
        | Omd.Emph v
        | Omd.Paragraph v ->
            to_words v
        | Omd.Url (_, inner, title) -> split_words title @ to_words inner
        | Omd.Ref (_, _, title, _) -> split_words title
        | Omd.Ol l | Omd.Olp l | Omd.Ul l | Omd.Ulp l ->
            List.fold
              ~init:([] : string list)
              ~f:(fun accm elem -> accm @ to_words elem)
              l
        | _ -> [] )
        @ to_words tl
end

module Encoding = struct
  let to_string ~style t =
    match style with
    | `Raw -> In_channel.read_all (get_path t)
    | `Json -> Ezjsonm.to_string (to_json t)
    | `Yaml -> Yaml.to_string_exn (to_json t)
end

module Search = struct

  type strategy = Keys | Fulltext

  let title key note = String.equal key (get_title note)

  let tags key note =
    let tags = get_tags note in
    List.count ~f:(fun tag -> String.equal key tag) tags > 0

  let of_strings strategy args =
    match strategy with
    | Keys ->
        List.map
          ~f:(fun arg ->
            let has_title = title arg in
            let has_tag = tags arg in
            fun note -> has_title note || has_tag note)
          args
    | Fulltext -> failwith "not implemented"

  let find_one ?(strategy = Keys) ~args notes =
    let filters = of_strings strategy args in
    List.find
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes

  let find_many ?(strategy = Keys) ~args notes =
    let filters = of_strings strategy args in
    List.filter
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes
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
                        (List.length (Util.to_words note.markdown))
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
