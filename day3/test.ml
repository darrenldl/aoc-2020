let test_get_circular_index () =
  Alcotest.(check int)
    "test_get_circular_index" 0
    (Lib.get_circular_index 1 0 1)

let test_get_circular_index_2 () =
  Alcotest.(check int)
    "test_get_circular_index_2" 0
    (Lib.get_circular_index 1 0 2)

let test_get_circular_index_3 () =
  Alcotest.(check int)
    "test_get_circular_index_3" 1
    (Lib.get_circular_index 2 0 1)

let test_get_circular_index_4 () =
  Alcotest.(check int) "can get correct index" 1 (Lib.get_circular_index 2 0 3)

let test_get_circular_index_5 () =
  Alcotest.(check int) "can get correct index" 0 (Lib.get_circular_index 2 0 4)

let test_get_circular_index_6 () =
  Alcotest.(check int) "can get correct index" 0 (Lib.get_circular_index 2 1 1)

let () =
  let open Alcotest in
  run "test"
    [
      ( "fns",
        [
          test_case "test_get_circular_index" `Quick test_get_circular_index;
          test_case "test_get_circular_index_2" `Quick test_get_circular_index_2;
          test_case "test_get_circular_index_3" `Quick test_get_circular_index_3;
          test_case "test_get_circular_index_4" `Quick test_get_circular_index_4;
          test_case "test_get_circular_index_5" `Quick test_get_circular_index_5;
          test_case "test_get_circular_index_6" `Quick test_get_circular_index_6;
        ] );
    ]
