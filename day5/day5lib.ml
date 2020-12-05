open Base
open Printf
open Fixture

let invalid_char ch = failwith @@ sprintf "invalid char %c" ch

let row_char_to_bin = function 'F' -> 0 | 'B' -> 1 | _ as c -> invalid_char c

let col_char_to_bin = function 'L' -> 0 | 'R' -> 1 | _ as c -> invalid_char c

let int_of_binary_ints ints =
  let f i acc b = acc + (b * (2 ** i)) in
  List.foldi ~f ~init:0 (List.rev ints)

let chs_to_int char_to_bin chs =
  int_of_binary_ints @@ CCList.map char_to_bin chs

let row_chars_to_int = chs_to_int row_char_to_bin
let col_chars_to_int = chs_to_int col_char_to_bin

module Boarding_pass = struct
  let of_string s =
    let open CCList in
    let chars = String.to_list s in
    let row = row_chars_to_int (take 7 chars) in
    let col = col_chars_to_int (rev (take 3 (rev chars))) in
    (row * 8) + col
end

let find_soln_1 () =
  let open List in
  get_lines (fun s -> s)
  |> map ~f:Boarding_pass.of_string
  |> fold_left ~f:max ~init:0

let find_soln_2 () =
  let open List in
  get_lines (fun s -> s)
  |> map ~f:Boarding_pass.of_string
  |> sort ~compare:Int.compare
  |> fun ids ->
    find_mapi ~f:(fun i id ->
      let id' = id + 1 in
      let next = nth_exn ids (i+1) in
      if next = id' then None else Some id'
    ) ids
  |> fun id -> Option.value_exn id
