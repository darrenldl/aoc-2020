open Base

let get_circular_index length current inc = Int.rem (inc + current) length

let test_tree_hit ch = Char.equal ch '#'

let find_soln () =
  let rows = Fixture.get_lines String.to_list in
  let width = List.(length (hd_exn rows)) in
  let part_1_slide_x = 3 in
  let slide_toboggan (tob_x, tree_hits) (terrain_chars : char list) =
    let next_x = get_circular_index width tob_x part_1_slide_x in
    let is_tree_hit = List.nth_exn terrain_chars tob_x |> test_tree_hit in
    (next_x, tree_hits + if is_tree_hit then 1 else 0)
  in
  List.fold rows ~f:slide_toboggan ~init:(0, 0)
  |> fun (_, tree_hits) -> tree_hits
