open Base
(* open Printf *)
open Fixture

let responses_to_set =
  List.fold_left ~f:(fun init s ->
    let f acc x = Set.add acc x in
    String.to_list s |> List.fold_left ~init ~f
  ) ~init:(Set.empty (module Char))

let find_soln_1 () =
  let open CCList in
  get_newline_delimited_lines ()
  |> map responses_to_set
  |> map Set.length
  |> fold_left (+) 0

