[@@@warning "-32-26"]

module Example = struct
  let one = {|-L|F7
7S-7|
L|7||
-L-J|
L|-JF|}

  let two = {|7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ|}

  let three =
    {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........|}

  let four =
    {|.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...|}

  let five =
    {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L|}

  let six = {|-L.F7
7S7.|
L||.|
-LJ.|
L|.JF|}
end

module Grid = struct
  type t = char array array [@@deriving show]

  let nrows (grid : t) = Array.length grid

  let ncols (grid : t) = Array.length grid.(0)

  let get (grid : t) (pos : int * int) : char =
    let nrow = fst pos in
    let ncol = snd pos in
    if nrow < 0 || nrow >= nrows grid then
      '.'
    else (
      let row = grid.(nrow) in
      if ncol < 0 || ncol >= Array.length row then
        '.'
      else
        row.(ncol)
    )

  let startpos (grid : t) =
    let rows = nrows grid in
    let cols = ncols grid in
    let pos = ref (0, 0) in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        if grid.(i).(j) = 'S' then
          pos := i, j
        else
          ()
      done
    done;
    !pos

  let connections (c : char) =
    match c with
    | '|' -> [ -1, 0; 1, 0 ]
    | '-' -> [ 0, -1; 0, 1 ]
    | 'L' -> [ -1, 0; 0, 1 ]
    | 'J' -> [ -1, 0; 0, -1 ]
    | '7' -> [ 1, 0; 0, -1 ]
    | 'F' -> [ 1, 0; 0, 1 ]
    | 'S' -> [ -1, 0; 1, 0; 0, -1; 0, 1 ]
    | _ -> []

  let nbrs (grid : t) (pos1, pos2) =
    let open CCList in
    let rawnbrs =
      connections (get grid (pos1, pos2)) >|= fun (d1, d2) ->
      d1 + pos1, d2 + pos2
    in
    CCList.filter
      (fun (p1, p2) ->
        CCList.exists
          (fun (a, b) -> a = pos1 && b = pos2)
          (CCList.map
             (fun (x, y) -> x + p1, y + p2)
             (connections (get grid (p1, p2)))))
      rawnbrs

  let parse (str : string) : t =
    let lines =
      try CCIO.(with_in str read_lines_l)
      with Sys_error _ -> String.split_on_char '\n' str
    in
    CCList.(
      let+ x = lines in
      x |> String.to_seq |> Array.of_seq)
    |> Array.of_list
end

module Part1 = struct
  let ans (grid : Grid.t) =
    let initpos = Grid.startpos grid in
    let curpos = ref @@ List.hd @@ Grid.nbrs grid initpos in
    let seen = ref [ initpos; !curpos ] in
    while
      CCList.filter (fun x -> not (CCList.mem x !seen)) (Grid.nbrs grid !curpos)
      != []
    do
      let next =
        CCList.filter
          (fun x -> not (CCList.mem x !seen))
          (Grid.nbrs grid !curpos)
        |> List.hd
      in
      curpos := next;
      seen := !seen @ [ next ]
    done;
    !seen, CCList.length !seen / 2
end

module Part2 = struct
  (* Using Pick's theorem and the Shoelace formula! *)

  let area points =
    let rec aux points =
      match points with
      | [] -> 0
      | [ _ ] -> 0
      | (x1, y1) :: ((x2, y2) as p2) :: xs ->
        (x1 * y2) - (x2 * y1) + aux (p2 :: xs)
    in
    abs @@ (aux points / 2)

  let ans grid =
    let bdry_path, bdry_count = Part1.ans grid in
    let area = area (bdry_path @ [ Grid.startpos grid ]) in
    area - bdry_count + 1
end

let () =
  let grid = Grid.parse "input.txt" in
  let startpos = Grid.startpos grid in
  CCFormat.printf "Part 1: %d" (Part1.ans grid |> snd)

let () =
  let grid = Grid.parse "input.txt" in
  let startpos = Grid.startpos grid in
  CCFormat.printf "@.Part 2: %d@." (Part2.ans grid)
