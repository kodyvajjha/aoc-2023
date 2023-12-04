let _test =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}

module Syntax = struct
  type t = {
    id: int;
    winners: int list;
    have: int list;
  }
  [@@deriving show { with_path = false }]

  let parser =
    let open CCParse in
    let* line = lookahead all in
    if Slice.is_empty line then
      return None
    else
      let* id = skip_space *> string "Card" *> U.int <* string ":" in
      let* winners = sep ~by:white U.int <* string "|" in
      let* have = sep ~by:white U.int in
      let+ _endline = all_str in
      Some { id; winners; have }

  let _parse_string ~input =
    let open CCParse in
    let string_parser = each_line parser >|= CCList.keep_some in
    try parse_string_exn string_parser input
    with ParseError err ->
      failwith @@ CCFormat.sprintf "failed: %a" CCParse.Error.pp err

  let parse_file ~input =
    let open CCParse in
    let file_parser = each_line (parsing "line" parser) >|= CCList.keep_some in
    match parse_file_e file_parser input with
    | Ok ans -> ans
    | Error err ->
      failwith @@ CCFormat.sprintf "failed: %a" CCParse.Error.pp err
end

let () =
  CCFormat.printf "%a" (CCList.pp Syntax.pp)
    (Syntax.parse_file ~input:"input.txt")
(* (Syntax._parse_string ~input:_test) *)
