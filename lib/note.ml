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

let tokenize t =
  let rec _tokenize markdown =
    List.fold
      ~init:([] : string list)
      ~f:(fun accm entry ->
        match entry with
        | Omd.Text text -> accm @ String.split_on_chars ~on:[ ' ' ] text
        | Omd.H1 header -> accm @ _tokenize header
        | Omd.H2 header -> accm @ _tokenize header
        | Omd.H3 header -> accm @ _tokenize header
        | Omd.H4 header -> accm @ _tokenize header
        | Omd.H5 header -> accm @ _tokenize header
        | Omd.H6 header -> accm @ _tokenize header
        | Omd.Paragraph paragraph -> accm @ _tokenize paragraph
        | Omd.Emph text -> accm @ _tokenize text
        | Omd.Bold text -> accm @ _tokenize text
        | Omd.Ul markdown_list ->
            let inner =
              List.fold ~init:[]
                ~f:(fun accm entry -> accm @ _tokenize entry)
                markdown_list
            in
            accm @ inner
        | Omd.Ol markdown_list ->
            let inner =
              List.fold ~init:[]
                ~f:(fun accm entry -> accm @ _tokenize entry)
                markdown_list
            in
            accm @ inner
        | Omd.Ulp markdown_list ->
            let inner =
              List.fold ~init:[]
                ~f:(fun accm entry -> accm @ _tokenize entry)
                markdown_list
            in
            accm @ inner
        | Omd.Olp markdown_list ->
            let inner =
              List.fold ~init:[]
                ~f:(fun accm entry -> accm @ _tokenize entry)
                markdown_list
            in
            accm @ inner
        | _ -> accm)
      markdown
  in

  _tokenize t.markdown

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
            (* TODO Sexp, ...?? *)
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

module Filter = struct
  type strategy = Keys | Path | Subset

  let title key note = String.equal key (get_title note)

  let tags key note =
    let tags = get_tags note in
    List.count ~f:(fun tag -> String.equal key tag) tags > 0

  let subset entry note =
    let rec is_subset json subset =
      let open Json_derivers.Jsonm in
      if compare (Ezjsonm.wrap json) (Ezjsonm.wrap subset) = 0 then true
      else
        match json with
        | `A lst -> List.count ~f:(fun entry -> is_subset entry subset) lst > 0
        | `O dct ->
            List.count ~f:(fun (_, entry) -> is_subset entry subset) dct > 0
        | _ -> false
    in

    match note.frontmatter with
    | Some fm ->
        (* check frontmatter first *)
        if is_subset (Ezjsonm.value fm) entry then true
        else is_subset (get_data note) entry
    | None -> is_subset (get_data note) entry

  let jsonpath ?(other = None) path note =
    let open Json_derivers.Jsonm in
    let open Jsonpath in
    let filter_data =
      List.count
        ~f:(fun value ->
          match other with
          | Some doc -> compare (Ezjsonm.wrap value) (Ezjsonm.wrap doc) > 0
          | None -> true)
        (Jsonm.select path (get_data note))
      > 0
    in
    match note.frontmatter with
    | Some fm ->
        if
          List.count
            ~f:(fun value ->
              match other with
              | Some doc -> compare (Ezjsonm.wrap value) (Ezjsonm.wrap doc) > 0
              | None -> true)
            (Jsonm.select path (Ezjsonm.value fm))
          > 0
        then true
        else filter_data
    | None -> filter_data

  let of_strings (strategy : strategy) (args : string list) =
    match strategy with
    | Path ->
        if List.length args % 2 = 0 then
          List.filter_mapi
            ~f:(fun i arg ->
              if i % 2 = 0 then
                let path = Jsonpath.of_string arg in
                let doc_string = List.nth_exn args (i + 1) in
                let other = Ezjsonm.value (Ezjsonm.from_string doc_string) in
                Some (jsonpath ~other:(Some other) path)
              else None)
            args
        else List.map ~f:(fun arg -> jsonpath (Jsonpath.of_string arg)) args
    | Keys ->
        List.map
          ~f:(fun arg ->
            let has_title = title arg in
            let has_tag = tags arg in
            fun note -> has_title note || has_tag note)
          args
    | Subset -> List.map ~f:(fun arg -> subset (Ezjsonm.from_string arg)) args

  let find_one filters notes =
    List.find
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes

  let find_one_with_paths filters notes =
    List.find
      ~f:(fun (note, _) ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes

  let find_many filters notes =
    List.filter
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes
end
