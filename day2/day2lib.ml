let parse_policy str =
  let policy_fmt = regexp "(\\d+)-(\\d+)\\s+([a-zA-Z]): (.+)" in
  exec ~rex:policy_fmt str |> get_substrings |> function
  | [| _; smin; smax; ch; pw |] ->
      (int_of_string smin, int_of_string smax, ch.[0], pw)
  | _ -> raise Not_found

(* part b *)
let test_is_compliant (a, b, ch, pw) =
  List.filter (fun x -> pw.[x - 1] |> Char.equal ch) [ a; b ] |> List.length
  |> fun len -> len = 1

let find_soln () =
  Fixture.get_lines parse_policy |> List.filter test_is_compliant |> List.length

(* part a *)
(* let test_is_compliant (min, max, ch, pw) =
  let count = ref 0 in
  let inc () = count := !count + 1 in
  String.iter (fun ch' -> if Char.equal ch ch' then inc ()) pw;
  !count >= min && !count <= max *)
