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

  let find_one ?(strategy=Keys) ~args notes =
    let filters = of_strings strategy args in
    List.find
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes

  let find_one_with_paths ?(strategy=Keys) ~args notes =
    let filters = of_strings strategy args in
    List.find
      ~f:(fun (note, _) ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes

  let find_many ?(strategy=Keys) ~args notes =
    let filters = of_strings strategy args in
    List.filter
      ~f:(fun note ->
        List.count ~f:(fun filter -> filter note) filters > 0
        || List.length filters = 0)
      notes
end

module Display = struct
  (* TODO: Colourized tags *)
  open ANSITerminal

  type style = Fancy | Simple

  (* three string columns where col 2 and 3 float to the right *)
  let three_col_string max1 max2 max3 col1 col2 col3 =
    let (s1, t1), (s2, t2), (s3, t3) = (col1, col2, col3) in
    let width, _ = size () in
    (* left side *)
    let l1 = String.concat [ t1; String.make (max1 - String.length t1) ' ' ] in
    (* right side *)
    let r1 = String.concat [ String.make (max2 - String.length t2) ' '; t2 ] in
    let r2 = String.concat [ String.make (max3 - String.length t3) ' '; t3 ] in
    let padding =
      String.make
        (width - (String.length l1 + String.length r1 + String.length r2 + 2))
        ' '
    in
    String.concat
      [
        sprintf s1 "%s" l1;
        padding;
        sprintf s2 "%s" r1;
        "  ";
        sprintf s3 "%s" r2;
      ]

  let print_short ~style notes =
    let columns =
      [ (([ Bold ], "title"), ([ Bold ], "tags"), ([ Bold ], "words")) ]
      @ List.map
          ~f:(fun note ->
            let title = get_title note in
            let tags = String.concat ~sep:"|" (get_tags note) in
            let word_count = Core.sprintf "%d" (List.length (tokenize note)) in
            (([], title), ([], tags), ([], word_count)))
          notes
    in
    match style with
    | Simple ->
        List.iter
          ~f:(fun (col1, col2, col3) ->
            let (_, text1), (_, text2), (_, text3) = (col1, col2, col3) in
            print_endline (Core.sprintf "%s %s %s" text1 text2 text3))
          columns
    | Fancy ->
        let max1, max2, max3 =
          List.fold ~init:(0, 0, 0)
            ~f:(fun accm pair ->
              let (_, text1), (_, text2), (_, text3) = pair in
              let col1, col2, col3 =
                (String.length text1, String.length text2, String.length text3)
              in
              let max1, max2, max3 = accm in
              let max1 = if col1 > max1 then col1 else max1 in
              let max2 = if col2 > max2 then col2 else max2 in
              let max3 = if col3 > max3 then col3 else max3 in
              (max1, max2, max3))
            columns
        in
        List.iter
          ~f:(fun pair ->
            let line =
              three_col_string max1 max2 max3 (fst3 pair) (snd3 pair)
                (trd3 pair)
            in
            print_endline line)
          columns
end
