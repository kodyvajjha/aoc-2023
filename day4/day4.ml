[@@@warning "-32"]

let _test =
  {|
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}

module Card = struct
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
    let string_parser =
      each_line (parsing "string" parser) >|= CCList.keep_some
    in
    try parse_string_exn string_parser input
    with ParseError err ->
      failwith @@ CCFormat.sprintf "failed: %a" CCParse.Error.pp err

  let parse_file ~input =
    let open CCParse in
    let file_parser = each_line (parsing "file" parser) >|= CCList.keep_some in
    match parse_file_e file_parser input with
    | Ok ans -> ans
    | Error err ->
      failwith @@ CCFormat.sprintf "failed: %a" CCParse.Error.pp err
end

module Part1 = struct
  let rec score (l : int list) =
    match l with
    | [] -> 0
    | [ _ ] -> 1
    | _ :: sc -> 2 * score sc

  let score_list (card : Card.t) =
    let have = card.have in
    let winning = card.winners in
    let matches =
      CCList.filter (fun x -> CCList.exists (fun y -> y = x) have) winning
    in
    score matches

  let ans cards = CCList.fold_left ( + ) 0 (CCList.map score_list cards)
end

module Part2 = struct
  let matches (card : Card.t) =
    ( card.id,
      CCList.filter
        (fun x -> CCList.exists (fun y -> y = x) card.have)
        card.winners
      |> CCList.length )

  let process_matches (tbl : (int, int) Hashtbl.t) ((id, matches) : int * int) =
    for m = 1 to matches do
      let copies = Hashtbl.find tbl id in
      CCHashtbl.incr tbl ~by:copies (id + m)
    done

  let ans cards =
    let tbl = Hashtbl.create 200 in
    for id = 1 to CCList.length cards do
      Hashtbl.add tbl id 1
    done;
    let matches = CCList.map matches cards in
    List.iter (process_matches tbl) matches;
    CCList.fold_left ( + ) 0 (List.map snd (CCHashtbl.to_list tbl))
end

let () =
  let card = Card.parse_file ~input:"input.txt" in
  CCFormat.printf "Part 1 : %d" (Part1.ans card)

let () =
  let cards = Card.parse_file ~input:"input.txt" in
  CCFormat.printf "@.Part 2 : %d" (Part2.ans cards)
