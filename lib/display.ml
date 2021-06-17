open Core
open ANSITerminal

type cell = string * int * int

and row = cell list

type cells = (string * int * int) list list

module Tabular = struct
  let paint_tag (styles : Config.StylePair.t list) text : string =
    match
      List.find ~f:(fun entry -> String.equal entry.pattern text) styles
    with
    | Some entry -> sprintf entry.styles "%s" text
    | None -> sprintf [ Foreground Default ] "%s" text

  let to_cells ?(paint = false) ~columns ~styles (notes : Note.note list) =
    let header =
      List.map
        ~f:(fun column ->
          let text_value = Config.Column.to_string column in
          let text_length = String.length text_value in
          let text_value =
            if paint then sprintf [ Bold; Underlined ] "%s" text_value
            else text_value
          in
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
                      let text_value = note.frontmatter.title in
                      (text_value, String.length text_value, default_padding)
                  | `Description ->
                      let text_value = note.frontmatter.description in
                      (text_value, String.length text_value, default_padding)
                  | `Slug ->
                      let text_value =
                        match note.slug with
                        | Some slug -> slug |> Slug.shortname
                        | None -> "??"
                      in
                      (text_value, String.length text_value, default_padding)
                  | `Tags ->
                      let text_value =
                        String.concat ~sep:"|" note.frontmatter.tags
                      in
                      let text_length = String.length text_value in
                      let tags = note.frontmatter.tags in
                      let tags =
                        if paint then
                          List.map ~f:(fun tag -> paint_tag styles tag) tags
                        else tags
                      in
                      let text_value = String.concat ~sep:"|" tags in
                      (text_value, text_length, default_padding)
                  | `LineCount ->
                      let count =
                        note.content |> String.split_lines |> List.length
                      in
                      let text_value = count |> Core.sprintf "%d" in
                      (text_value, String.length text_value, default_padding))
                columns;
            ])
        notes
    in
    [ header ] @ note_cells

  let fixed cells =
    (* find the maximum cell length per column *)
    let maximum_values =
      List.fold ~init:[]
        ~f:(fun accm row ->
          List.mapi
            ~f:(fun i col ->
              let col_length = snd3 col in
              let current_max =
                match List.nth accm i with Some len -> len | None -> 0
              in
              if col_length > current_max then col_length + 2 else current_max)
            row)
        cells
    in
    maximum_values

  let fixed_right cells =
    let widths = cells |> fixed in
    let term_width, _ = size () in
    let _, right = List.split_n widths 1 in
    let col_one = List.nth_exn widths 0 in
    [ col_one + (term_width - List.fold ~init:5 ~f:( + ) widths) ] @ right

  let apply ~widths cells =
    (* let maximums = fixed_spacing cells in *)
    let cells =
      List.map
        ~f:(fun row ->
          List.mapi
            ~f:(fun i entry ->
              let max = List.nth_exn widths i in
              let text, length, padding = entry in
              let padding = padding + (max - length) in
              let padding = if padding > 0 then padding else 0 in
              (text, length, padding))
            row)
        cells
    in
    List.fold ~init:[]
      ~f:(fun accm row ->
        accm
        @ [
            List.fold ~init:""
              ~f:(fun accm cell ->
                let text, _, padding = cell in
                String.concat [ accm; text; String.make padding ' ' ])
              row;
          ])
      cells

  let simple cells =
    let lines =
      List.slice
        (cells |> List.map ~f:(fun row -> row |> List.hd_exn |> fst3))
        1 0
      |> String.concat ~sep:"\n"
    in
    "\n" ^ lines ^ "\n"

  let fixed cells =
    let lines =
      apply ~widths:(cells |> fixed) cells |> String.concat ~sep:"\n"
    in
    "\n" ^ lines ^ "\n"

  let wide cells =
    let lines =
      apply ~widths:(cells |> fixed_right) cells |> String.concat ~sep:"\n"
    in
    "\n" ^ lines ^ "\n"
end

module Hierarchical = struct
  type tree = Tree of (string * tree list)

  let get_padding = function true -> "│  " | false -> "   "

  let get_edge = function true -> "└──" | false -> "├──"

  let fill ~last ~state =
    match List.length state with
    | 0 -> []
    | 1 -> [ last |> get_edge ]
    | len ->
        let state = List.slice state 0 (len - 1) in
        let padding = List.map ~f:get_padding state in
        List.append padding [ last |> get_edge ]

  let rec to_lines ?(state = []) ?(last = false) next =
    let (Tree next) = next in
    let title, children = next in
    match List.length children with
    | 0 ->
        (* leaf *)
        List.append (fill ~last ~state) [ title; "\n" ]
    | n_children ->
        (* node *)
        List.foldi
          ~init:
            [ List.append (fill ~last ~state) [ title; "\n" ] |> String.concat ]
          ~f:(fun i accm node ->
            let is_last = Int.equal i (n_children - 1) in
            let state = List.append state [ phys_equal is_last false ] in
            let lines = to_lines ~state ~last:is_last node in
            List.append accm lines)
          children

  let to_string tree = "\n" ^ (tree |> to_lines |> String.concat)
end

let rec convert_tree tree =
  let (Note.Tree (note, others)) = tree in
  let title = note.frontmatter.title in
  let title = "[" ^ title ^ "]" in
  Hierarchical.Tree (title, List.map ~f:convert_tree others)

let to_string ?(style = `Tree) ?(columns = []) ?(styles = []) notes =
  match style with
  | `Tree -> notes |> convert_tree |> Hierarchical.to_string
  | `Simple ->
      notes |> Note.flatten ~accm:[] |> Tabular.to_cells ~columns ~styles
      |> Tabular.simple
  | `Fixed ->
      notes |> Note.flatten ~accm:[] |> Tabular.to_cells ~columns ~styles
      |> Tabular.fixed
  | `Wide ->
      notes |> Note.flatten ~accm:[] |> Tabular.to_cells ~columns ~styles
      |> Tabular.wide
