open Core
open ANSITerminal

type cell = string * int * int

type row = cell list

module Tree = struct
  type t = Tree of (string * t list)

  let fill ~last ~state =
    let get_padding = function true -> "│  " | false -> "   " in
    let get_edge = function true -> "└──" | false -> "├──" in
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

  let to_string t =
    let result = t |> to_lines |> String.concat in
    "\n" ^ result
end

module Cell = struct
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
end
