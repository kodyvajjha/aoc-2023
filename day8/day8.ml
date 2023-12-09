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

  type nodes = (string * (string * string)) list [@@deriving show]

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
    let rec aux steps moves start =
      if start = "ZZZ" then
        steps
      else (
        match moves with
        | [] -> aux steps map.moves start
        | l :: ls ->
          (match l with
          | Directions.Left -> aux (steps + 1) ls (Hashtbl.find tbl start |> fst)
          | Directions.Right ->
            aux (steps + 1) ls (Hashtbl.find tbl start |> snd))
      )
    in
    aux 0 map.moves "AAA"
end

module Part2 = struct
  let rec gcd u v =
    if v <> 0 then
      gcd v (u mod v)
    else
      abs u

  let lcm m n =
    match m, n with
    | 0, _ | _, 0 -> 0
    | m, n -> abs (m * n) / gcd m n

  let ans (dirmap : Directions.t) =
    let ends_with_char str c =
      let len = String.length str in
      if len = 0 then
        false
      else
        str.[len - 1] = c
    in
    let tbl = dirmap.nodes |> CCHashtbl.of_list in
    let starts =
      CCList.(
        filter (fun (x, _) -> ends_with_char x 'A') dirmap.nodes >|= fun x ->
        fst x)
    in
    let rec aux steps moves start =
      if ends_with_char start 'Z' then
        steps
      else (
        match moves with
        | [] -> aux steps dirmap.moves start
        | l :: ls ->
          (match l with
          | Directions.Left -> aux (steps + 1) ls (Hashtbl.find tbl start |> fst)
          | Directions.Right ->
            aux (steps + 1) ls (Hashtbl.find tbl start |> snd))
      )
    in
    let steps = CCList.map (aux 0 dirmap.moves) starts in
    let nsteps = CCList.fold_left lcm 1 steps in
    nsteps
end

let () =
  match CCParse.parse_file_e Parse.directions "input.txt" with
  (* | Ok dirs -> CCFormat.printf "%a" Directions.pp dirs *)
  | Ok dirs -> CCFormat.printf "%d" (Part1.ans dirs)
  | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err

let () =
  match CCParse.parse_file_e Parse.directions "input.txt" with
  (* | Ok dirs -> CCFormat.printf "%a" Directions.pp dirs *)
  | Ok dirs -> CCFormat.printf "@.%d" (Part2.ans dirs)
  | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err
