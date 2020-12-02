let line_stream_of_channel channel =
  Stream.from (fun _ ->
      try Some (input_line channel) with End_of_file -> None)

let get_lines (parse_line : string -> 'a) =
  let lines = ref [] in
  let in_channel = open_in Sys.argv.(1) in
  try
    Stream.iter
      (fun line -> lines := parse_line line :: !lines)
      (line_stream_of_channel in_channel);
    close_in in_channel;
    !lines |> List.rev
  with e ->
    close_in in_channel;
    raise e
