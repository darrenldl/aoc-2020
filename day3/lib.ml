open Base

let part_2_dx_dy = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]

let get_circular_index length current inc = Int.rem (inc + current) length

let test_tree_hit ch = Char.equal ch '#'

let get_tree_hits all_rows width (dx, dy) =
  let slide_toboggan (tob_x, tree_hits) (terrain_chars : char list) =
    let next_x = get_circular_index width tob_x dx in
    let is_tree_hit = List.nth_exn terrain_chars tob_x |> test_tree_hit in
    (next_x, tree_hits + if is_tree_hit then 1 else 0)
  in
  List.filteri ~f:(fun i _ -> Int.(equal (rem i dy) 0)) all_rows
  |> List.fold ~f:slide_toboggan ~init:(0, 0)
  |> fun (_, tree_hits) -> tree_hits

let as_tuple a b = (a, b)

let find_soln () =
  Fixture.get_lines String.to_list |> fun rows ->
  (as_tuple rows @@ List.(length (hd_exn rows))) |> fun (rows, width) ->
  List.map ~f:(get_tree_hits rows width) part_2_dx_dy
  |> List.fold ~init:1 ~f:Int.( * )
