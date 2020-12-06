open Base
open Fixture

type char_count = (char, int, Char.comparator_witness) Map.t

let inc_opt = function Some x -> x + 1 | None -> 1

let counts_of_all_yes group =
  let open CCList in
  let char_counter = Map.empty (module Char) in
  let num_members = length group in
  let inc_char = Map.update ~f:inc_opt in
  let count_of_chars =
    fold_left
      (fun init answer -> String.fold ~init ~f:inc_char answer)
      char_counter group
  in
  let inc_if_all_yes ~key:_ ~data total =
    if data = num_members then total + 1 else total
  in
  Map.fold ~init:0 ~f:inc_if_all_yes count_of_chars

let to_counts_of_all_yes groups = List.map ~f:counts_of_all_yes groups

let find_soln_2 () =
  get_newline_delimited_lines ()
  |> to_counts_of_all_yes
  |> List.fold_left ~f:( + ) ~init:0

(* let responses_to_set =
  List.fold_left
    ~f:(fun init s ->
      let f acc x = Set.add acc x in
      String.to_list s |> List.fold_left ~init ~f)
    ~init:(Set.empty (module Char)) *)

(* let find_soln_1 () =
  let open CCList in
  get_newline_delimited_lines ()
  |> map responses_to_set
  |> map Set.length
  |> fold_left (+) 0 *)
