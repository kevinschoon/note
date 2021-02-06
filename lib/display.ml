open Core
open ANSITerminal

type cell = string * int * int

type row = cell list

let fixed_spacing cells =
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

let fix_right cells =
  let widths = fixed_spacing cells in
  let term_width, _ = size () in
  let _, right = List.split_n widths 1 in
  let col_one = List.nth_exn widths 0 in
  [ col_one + (term_width - List.fold ~init:5 ~f:( + ) widths) ] @ right

let apply cells widths =
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

let to_stdout ~style cells =
  match style with
  | `Simple ->
      List.iter
        ~f:(fun cell ->
          print_endline
            (let value = List.nth_exn cell 0 in
             let text = fst3 value in
             text))
        cells
  | `Fixed -> List.iter ~f:print_endline (apply cells (fixed_spacing cells))
  | `Wide -> List.iter ~f:print_endline (apply cells (fix_right cells))
