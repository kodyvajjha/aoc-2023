let _test =
  {|
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}

let () = Printexc.record_backtrace true

let parse_with_error_handling lexbuf =
  try Inputgram.inputfile Inputlex.token lexbuf with
  | Inputgram.Error ->
    print_endline ("Syntax error at " ^ Lexing.lexeme lexbuf);
    exit 1
  | Inputlex.SyntaxError msg ->
    print_endline ("Lexer error: " ^ msg);
    exit 1

module Part1 = struct
  open Syntax

  let cube_not_possible (c : cubes) =
    match c with
    | { red = Some r; green = Some g; blue = Some b } ->
      r > 12 || g > 13 || b > 14
    | { red = None; green = Some g; blue = Some b } -> g > 13 || b > 14
    | { red = Some r; green = None; blue = Some b } -> r > 12 || b > 14
    | { red = Some r; green = Some g; blue = None } -> r > 12 || g > 13
    | { red = Some r; green = None; blue = None } -> r > 12
    | { red = None; green = Some g; blue = None } -> g > 13
    | { red = None; green = None; blue = Some b } -> b > 14
    | { red = None; green = None; blue = None } -> failwith "invalid cube"

  let game_not_possible (g : game) = CCList.exists cube_not_possible g.grabs

  let ans (r : record) =
    let games = CCList.filter (fun g -> not (game_not_possible g)) r in
    CCList.fold_left ( + ) 0
      CCList.(
        let+ g = games in
        g.id)
end

let () =
  let ic = open_in "./input.txt" in
  let lexbuf = Lexing.from_channel ic in
  let games : Syntax.record = parse_with_error_handling lexbuf in
  let games_possible =
    CCList.filter (fun g -> not (Part1.game_not_possible g)) games
  in
  CCFormat.printf "@.%d" (Part1.ans games_possible)
