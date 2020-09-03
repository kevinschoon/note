open Core

type t = { frontmatter : Ezjsonm.t option; markdown : Omd.t }

let build ?(tags = []) ?(content = "") title =
  let frontmatter =
    Some
      (Ezjsonm.dict
         [ ("title", Ezjsonm.string title); ("tags", Ezjsonm.strings tags) ])
  in
  let markdown = Omd.of_string content in
  { frontmatter; markdown }

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
  (* Since we couldn't determine the title from frontmatter now we will infer the title by looking at the markdown *)
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

let get_tags t =
  match t.frontmatter with
  | Some fm -> (
      match Ezjsonm.find_opt (Ezjsonm.value fm) [ "tags" ] with
      | Some v -> Ezjsonm.get_strings v
      | None -> [] )
  | None -> []

let get_data (t : t) =
  let data : Ezjsonm.value list =
    List.filter_map
      ~f:(fun entry ->
        match entry with
        | Code (language, data) | Code_block (language, data) -> (
            match language with
            | "JSON" | "Json" | "json" ->
                Some (Ezjsonm.value (Ezjsonm.from_string data))
            | "YAML" | "Yaml" | "yaml" -> Some (Yaml.of_string_exn data)
            (* TODO Sexp, ...?? *)
            | _ -> None )
        | _ -> None)
      t.markdown
  in
  data

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
      ("data", Ezjsonm.list (fun x -> x) (get_data t));
    ]

let to_string t =
  match t.frontmatter with
  | Some fm ->
      let front_matter = Yaml.to_string_exn (Ezjsonm.value fm) in
      String.concat ~sep:"\n"
        [ "---"; front_matter; "---"; Omd.to_text t.markdown ]
  | None -> Omd.to_text t.markdown

let of_string data =
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
    { frontmatter; markdown }
  else
    let frontmatter = None in
    let markdown = Omd.of_string data in
    { frontmatter; markdown }

let filter_title ~keys note = 
    (List.count ~f:(fun key -> String.equal key (get_title note)) keys) > 0

let filter_tags ~keys note =
    let tags = (get_tags note) in
    List.count ~f: (fun tag -> List.mem ~equal:(String.equal) keys tag) tags > 0

let filter ?(keys = []) (note : t) =
  if List.length keys = 0 then true
  else
    (filter_title ~keys note) || (filter_tags ~keys note)
