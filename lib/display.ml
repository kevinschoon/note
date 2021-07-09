open Core

type cell = string * (string -> string)

and row = cell list

module Tabular = struct
  let maximum_lengths rows =
    rows
    |> List.foldi ~init:[] ~f:(fun index accm row ->
           let row_maximums =
             row |> List.map ~f:(fun cell -> fst cell |> String.length)
           in
           if Int.equal index 0 then row_maximums
           else List.map2_exn accm row_maximums ~f:Int.max)

  let fixed rows =
    let dimensions = rows |> maximum_lengths in
    let lines =
      rows
      |> List.fold ~init:[] ~f:(fun accm row ->
             let line =
               row
               |> List.foldi ~init:"" ~f:(fun index line cell ->
                      let content, paint_fn = cell in
                      let padding =
                        List.nth_exn dimensions index
                        - String.length content + 1
                      in
                      let padding = String.make padding ' ' in
                      let line = line ^ (content |> paint_fn) ^ padding in
                      line)
             in

             line :: accm)
    in
    lines |> List.rev

  let wide ?(pivot = 1) ~width rows =
    let dimensions = rows |> maximum_lengths in
    let left, right =
      rows
      |> List.fold ~init:([], []) ~f:(fun accm row ->
             let left, right = List.split_n row pivot in
             (left :: fst accm, right :: snd accm))
    in
    let left = left |> fixed in
    let right = right |> fixed in
    let column_length = dimensions |> List.reduce_exn ~f:( + ) in
    let n_columns = dimensions |> List.length in
    let total_length = width - (column_length + n_columns) in
    let padding = String.make total_length ' ' in
    List.map2_exn left right ~f:(fun left right ->
        String.concat [ left; padding; right ])
    |> List.rev

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
