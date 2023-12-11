[@@@warning "-32-26"]

let () = Printexc.record_backtrace true

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
    CCFormat.printf "curpos : %a@.nbrs: %a@.seen: %a@."
      CCFormat.Dump.(pair int int)
      !curpos
      CCFormat.Dump.(list (pair int int))
      (Grid.nbrs grid !curpos)
      CCFormat.Dump.(list (pair int int))
      !seen;
    while !curpos != initpos do
      (* for _ = 1 to 3 do *)
      CCFormat.printf "nbrs of curpos : %a@."
        CCFormat.Dump.(list (pair int int))
        (Grid.nbrs grid !curpos);

      let next =
        CCList.filter
          (fun x -> not (CCList.mem x !seen))
          (Grid.nbrs grid !curpos)
        |> List.hd
      in
      CCFormat.printf "filtered : %a@." CCFormat.Dump.(pair int int) next;
      curpos := next;
      seen := !seen @ [ next ];
      CCFormat.printf "seen : %a@." CCFormat.Dump.(list (pair int int)) !seen
    done;
    !seen
end

let () =
  let grid = Grid.parse Example.one in
  let startpos = Grid.startpos grid in
  CCFormat.printf "@.Grid: %a@.Current queue: %a" Grid.pp grid
    CCFormat.Dump.(list (pair int int))
    (Part1.ans grid)
