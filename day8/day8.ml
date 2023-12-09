[@@@warning "-32"]

let test = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

module Directions = struct
  type moves = move list

  and move =
    | Left
    | Right
  [@@deriving show { with_path = false }]

  type nodes = (string * (string * string)) list
  [@@deriving show { with_path = false }]

  type t = {
    moves: moves;
    nodes: nodes;
  }
  [@@deriving show { with_path = false }]
end

module Parse = struct
  let moves : Directions.moves CCParse.t =
    let open CCParse in
    let* chars = many_until ~until:endline any_char in
    pure
    @@ CCList.map
         (function
           | 'R' -> Directions.Right
           | 'L' -> Directions.Left
           | c -> failwith (CCFormat.sprintf "illegal character %c" c))
         chars

  let node =
    let open CCParse in
    let* next = lookahead all in
    if Slice.is_empty next then
      pure None
    else
      let* here = U.word <* space <* string "=" <* space in
      let* left = char '(' *> U.word <* skip (char ',') in
      let+ right = skip_space *> U.word <* skip (char ')') in
      Some (here, (left, right))

  let nodes = CCParse.(each_line node >|= CCList.keep_some)

  let directions : Directions.t CCParse.t =
    let open CCParse in
    let* moves = moves in
    let* _newline = endline in
    let+ nodes = nodes in
    { Directions.moves; nodes }
end

module Part1 = struct
  let ans (map : Directions.t) =
    let tbl = map.nodes |> CCHashtbl.of_list in
    let start, _next = map.nodes |> List.hd in
    let moves = map.moves in
    let seen = ref false in
    while true do
      ()
    done
end

let () =
  match CCParse.parse_string_e Parse.directions test with
  | Ok seeds -> CCFormat.printf "%a" Directions.pp seeds
  | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err
