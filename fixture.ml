let line_stream_of_channel : 'a -> string Stream.t =
 fun channel ->
  let read () = input_line channel in
  Stream.from (fun _ -> try Some (read ()) with End_of_file -> Option.None)

let get_lines : (string -> 'a) -> 'b list =
 fun parse_line ->
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
