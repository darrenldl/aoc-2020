open Base
open Printf

type string_map = string Map.M(String).t [@@deriving sexp]

let as_int_str_tpl s =
  String.to_list s
  |> List.fold
       ~f:(fun (num_chs, alpha_chs) ch ->
         if Char.is_digit ch then (ch :: num_chs, alpha_chs)
         else (num_chs, ch :: alpha_chs))
       ~init:([], [])
  |> fun (a, b) ->
  List.(rev a, rev b) |> fun (a, b) ->
  String.(Int.of_string (of_char_list a), of_char_list b)

let tap_assert v f =
  match f v with
  | true -> v
  | false -> raise_s (String.sexp_of_t "invalid travel value")

let as_byr v =
  tap_assert v (fun v -> Int.of_string v |> Int.between ~low:1920 ~high:2002)

let as_ecl v =
  let haystack = [| "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" |] in
  tap_assert v (fun v -> Array.count ~f:(String.equal v) haystack = 1)

let as_eyr v =
  tap_assert v (fun v -> Int.of_string v |> Int.between ~low:2020 ~high:2030)

let as_hcl v =
  tap_assert v (fun v ->
      let is_pnd_ok = Char.equal v.[0] '#' in
      let test_is_af ch =
        List.exists
          ~f:(fun ch' -> Char.equal ch' ch)
          [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ]
      in
      let test_is_code_char ch =
        Char.(is_digit ch || (is_lowercase ch && test_is_af ch))
      in
      let is_code_ok =
        List.tl_exn (String.to_list v) |> List.for_all ~f:test_is_code_char
      in
      let is_len_ok = Int.equal (String.length v) 7 in
      is_pnd_ok && is_code_ok && is_len_ok)

let as_hgt v =
  let assert_hgt v =
    as_int_str_tpl v |> function
    | h, "cm" -> Int.between ~low:150 ~high:193 h
    | h, "in" -> Int.between ~low:59 ~high:76 h
    | _ -> raise_s (String.sexp_of_t (sprintf "no height found for %s" v))
  in
  tap_assert v assert_hgt

let as_iyr v =
  tap_assert v (fun v -> Int.of_string v |> Int.between ~low:2010 ~high:2020)

let as_pid v =
  tap_assert v (fun v -> Int.of_string v |> fun _ -> String.length v = 9)

module North_pole = struct
  type t = {
    byr : string;
    ecl : string;
    eyr : string;
    hcl : string;
    hgt : string;
    iyr : string;
    pid : string;
  }
  [@@deriving fields, show]

  let of_map_opt (m : string_map) =
    let open Map in
    try
      Some
        {
          byr = find_exn m "byr" |> as_byr;
          ecl = find_exn m "ecl" |> as_ecl;
          eyr = find_exn m "eyr" |> as_eyr;
          hcl = find_exn m "hcl" |> as_hcl;
          hgt = find_exn m "hgt" |> as_hgt;
          iyr = find_exn m "iyr" |> as_iyr;
          pid = find_exn m "pid" |> as_pid;
        }
    with _ -> None
end

module Passport = struct
  type t = {
    cid : string;
    byr : string;
    iyr : string;
    eyr : string;
    hgt : string;
    hcl : string;
    ecl : string;
    pid : string;
  }
  [@@deriving fields]

  let of_map_opt (m : string_map) =
    let open Map in
    try
      Some
        {
          cid = find_exn m "cid";
          byr = find_exn m "byr" |> as_byr;
          ecl = find_exn m "ecl" |> as_ecl;
          eyr = find_exn m "eyr" |> as_eyr;
          hcl = find_exn m "hcl" |> as_hcl;
          hgt = find_exn m "hgt" |> as_hgt;
          iyr = find_exn m "iyr" |> as_iyr;
          pid = find_exn m "pid" |> as_pid;
        }
    with _ -> None
end

module Travel_papers = struct
  type t = Np of North_pole.t | Pp of Passport.t

  let split_on_whitespace = Str.split @@ Str.regexp "[ \t]+"

  let kvs_to_map fields =
    List.fold
      ~f:(fun m (key, data) -> Map.set m ~key ~data)
      ~init:(Map.empty (module String))
      fields

  let key_value_of_delim_string d s =
    Str.split_delim (Str.regexp d) s |> function
    | [ k; v ] -> (k, v)
    | _ -> raise_s (String.sexp_of_t "invalid travel doc key value")

  let key_values_of_string str =
    List.map ~f:(key_value_of_delim_string ":") (split_on_whitespace str)

  let of_map (m : string_map) : t Option.t =
    let len = Map.length m in
    match len with
    | 7 -> (
        North_pole.of_map_opt m |> function
        | Some np -> Some (Np np)
        | _ -> None )
    | 8 -> (
        Passport.of_map_opt m |> function Some pp -> Some (Pp pp) | _ -> None )
    | _ -> None

  let of_string str = key_values_of_string str |> kvs_to_map |> of_map
end


let to_single_line_rows lines =
  let open List in
  let filter_empty = filter ~f:(fun l -> not @@ String.is_empty l) in
  let join_str = String.concat ~sep:" " in
  let f singles raw_line =
    let line = String.rstrip raw_line in
    let is_terminator = String.length line = 0 in
    match is_terminator with
    | true -> [] :: singles
    | false -> (
        match singles with
        | head :: tail -> (line :: head) :: tail
        | _ -> singles )
  in
  fold ~f ~init:[ [] ] lines |> map ~f:join_str |> filter_empty

let parse_opt_travel_documents lines =
  to_single_line_rows lines |> List.map ~f:Travel_papers.of_string

let count_valid_documents docs = List.(filter ~f:Option.is_some docs |> length)

let find_soln () =
  Fixture.get_lines (fun s -> s)
  |> parse_opt_travel_documents
  |> count_valid_documents
