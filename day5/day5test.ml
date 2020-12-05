open Printf
open Base

let base_case_row_chars = [ 'F'; 'B'; 'F'; 'B'; 'B'; 'F'; 'F' ]

let base_case_row_ints = [ 0; 1; 0; 1; 1; 0; 0 ]

let test_row_char_to_bin () =
  Alcotest.(check (list int))
    "test_row_char_to_bin" base_case_row_ints
    (List.map ~f:Day5lib.row_char_to_bin base_case_row_chars)

let test_int_of_binary_ints () =
  Alcotest.(check int)
    "int_of_binary_ints" 44
    (Day5lib.int_of_binary_ints base_case_row_ints)

let test_base_example () =
  Alcotest.(check int)
    "test_base_example" 357
    (Day5lib.Boarding_pass.of_string "FBFBBFFRLR")

let test_base_col_template input output () =
  Alcotest.(check int)
    "test_base_example" output
    (Day5lib.col_chars_to_int input)

let test_base_row_template input output () =
  Alcotest.(check int)
    "test_base_example" output
    (Day5lib.row_chars_to_int input)

let cases =
  [
    ("BBFBFBB", 107, "LRR", 3);
    ("BFFFBBF", 70, "RRR", 7);
    (*row 70, column 7*)
    ("FFFBBBF", 14, "RRR", 7);
    (*row 14, column 7*)
    ("BBFFBBF", 102, "RLL", 4);
    (*row 102, column 4*)
  ]

let provided_cases =
  let open Alcotest in
  let open CCList in
  let f i (rowin, rowout, colin, colout) =
    [
      test_case
        (sprintf "test_base_case_col_%d" i)
        `Quick
        (test_base_col_template (String.to_list colin) colout);
      test_case
        (sprintf "test_base_case_row_%d" i)
        `Quick
        (test_base_row_template (String.to_list rowin) rowout);
    ]
  in
  flatten @@ mapi f cases

let () =
  let open Alcotest in
  run "test"
    [
      ( "fns",
        [
          test_case "test_int_of_binary_ints" `Quick test_int_of_binary_ints;
          test_case "test_row_char_to_bin" `Quick test_row_char_to_bin;
          test_case "test_base_example" `Quick test_base_example;
        ]
        @ provided_cases );
    ]
