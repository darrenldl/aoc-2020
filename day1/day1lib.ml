open CCList

let to_group l =
  flat_map (fun a -> flat_map (fun b -> map (fun c -> [ a; b; c ]) l) l) l

let find_2020_sum sum pairs =
  let equals_2020 digits = fold_left ( + ) 0 digits = sum in
  find equals_2020 pairs

let find_sum_groups ints = find_2020_sum 2020 @@ to_group ints

let find_soln () =
  find_sum_groups @@ Fixture.get_lines int_of_string |> function
  | [ a; b; c ] -> a * b * c
  | _ -> raise Not_found
