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

let () =
  let ic = open_in "./input.txt" in
  let lexbuf = Lexing.from_channel ic in
  let rounds = parse_with_error_handling lexbuf in
  CCFormat.printf "%a" Syntax.pp_record rounds
