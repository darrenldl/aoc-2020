open Base

type string_map = string Map.M(String).t [@@deriving sexp]

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
  [@@deriving fields]

  let of_map_opt (m : string_map) =
    let open Map in
    try
      Some
        {
          byr = find_exn m "byr";
          ecl = find_exn m "ecl";
          eyr = find_exn m "eyr";
          hcl = find_exn m "hcl";
          hgt = find_exn m "hgt";
          iyr = find_exn m "iyr";
          pid = find_exn m "pid";
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
          byr = find_exn m "byr";
          iyr = find_exn m "iyr";
          eyr = find_exn m "eyr";
          hgt = find_exn m "hgt";
          hcl = find_exn m "hcl";
          ecl = find_exn m "ecl";
          pid = find_exn m "pid";
        }
    with _ -> None
end

let rex_whitespace = Str.regexp "[ \t]+"

module Travel_papers = struct
  type t = Np of North_pole.t | Pp of Passport.t

  let split_on_whitespace = Str.split rex_whitespace

  let kvs_as_map fields =
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

  let of_string str = key_values_of_string str |> kvs_as_map |> of_map

  (* (fun m ->
     let p = Sexp.to_string (sexp_of_string_map m) in
     let res = of_map m in
     if Option.is_none res then
     Stdio.print_string @@ Printf.sprintf "%b\t%d- %s\n\n" (Option.is_some res) (Map.length m) p;
     res) *)
end

let of_non_empty_strings = List.filter ~f:(fun l -> not @@ String.is_empty l)

let parse_opt_travel_documents lines =
  let join_str = String.concat ~sep:" " in
  List.fold
    ~f:(fun groups raw_line ->
      let line = String.rstrip raw_line in
      let is_terminator = String.length line = 0 in
      match is_terminator with
      | true -> [] :: groups
      | false -> (
          match groups with
          | head :: tail -> (line :: head) :: tail
          | _ -> groups ))
    ~init:[ [] ] lines
  |> List.map ~f:join_str |> of_non_empty_strings
  |> List.map ~f:Travel_papers.of_string

let count_valid_documents docs =
  List.filter ~f:Option.is_some docs |> List.length

let find_soln () =
  Fixture.get_lines (fun s -> s)
  |> parse_opt_travel_documents |> count_valid_documents
