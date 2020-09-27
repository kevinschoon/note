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

let get_path t = Slug.get_path t.slug

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

module Display = struct
  (* TODO: Colourized tags *)

  open ANSITerminal

  type cell = string * ANSITerminal.style list

  type row = cell list

  let to_cells notes =
    [
      [
        ("title", [ Bold; Underlined ]);
        ("tags", [ Bold; Underlined ]);
        ("words", [ Bold; Underlined ]);
        ("slug", [ Bold; Underlined ]);
      ];
    ]
    @ List.fold ~init:[]
        ~f:(fun accm note ->
          let title = (get_title note, [ Reset ]) in
          let tags = (String.concat ~sep:"|" (get_tags note), [ Reset ]) in
          let word_count =
            (Core.sprintf "%d" (List.length (tokenize note)), [ Reset ])
          in
          let slug = (Slug.to_string note.slug, [ Reset ]) in
          accm @ [ [ title; tags; word_count; slug ] ])
        notes

  let fixed_spacing cells =
    (* find the maximum column length for all cells *)
    List.fold ~init:[]
      ~f:(fun accm row ->
        List.mapi
          ~f:(fun i col ->
            let col_length = String.length (fst col) in
            let current_max =
              match List.nth accm i with Some len -> len | None -> 0
            in
            if col_length > current_max then col_length + 1 else current_max)
          row)
      cells

  let fix_right widths =
    let term_width, _ = size () in
    let _, right = List.split_n widths 1 in
    let col_one = List.nth_exn widths 0 in
    [ col_one + (term_width - List.fold ~init:0 ~f:( + ) widths) ] @ right

  let apply widths cells =
    List.fold ~init:[]
      ~f:(fun accm row ->
        accm
        @ [
            List.foldi ~init:""
              ~f:(fun i accm cell ->
                let text = fst cell in
                let styles = snd cell in
                let cell_width = List.nth_exn widths i in
                let padding = cell_width - String.length text in
                String.concat
                  [ accm; sprintf styles "%s" text; String.make padding ' ' ])
              row;
          ])
      cells

  let to_stdout ~style notes =
    let cells = to_cells notes in
    match style with
    | `Simple ->
        List.iter
          ~f:(fun cell -> print_endline (fst (List.nth_exn cell 0)))
          cells
    | `Fixed ->
        List.iter ~f:print_endline (apply (fixed_spacing cells) cells)
    | `Wide ->
        List.iter ~f:print_endline
          (apply (fix_right (fixed_spacing cells)) cells)
end
