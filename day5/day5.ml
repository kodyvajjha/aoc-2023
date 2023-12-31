[@@@warning "-32"]

let test =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

type seeds = int list [@@deriving show]

type mapping = line list

and line = {
  destination_start: int;
  source_start: int;
  length: int;
}
[@@deriving show { with_path = false }]

type map = {
  source: string;
  destination: string;
  mapping: mapping;
}
[@@deriving show { with_path = false }]

type t = {
  seeds: seeds;
  maps: map list;
}
[@@deriving show { with_path = false }]

module Part1 = struct
  let apply mapping seed =
    let matches =
      let open CCList in
      let+ { destination_start; source_start; length } = mapping in
      if source_start <= seed && seed < source_start + length then
        Some (seed - source_start + destination_start)
      else
        None
    in

    match CCList.keep_some matches with
    | [] -> seed
    | [ x ] -> x
    | _ -> failwith "more than one match!"

  let ans almanac =
    let open CCList in
    let tbls =
      let+ map = almanac.maps in
      map.mapping
    in
    let ints =
      let+ seed = almanac.seeds in
      CCList.fold_left (fun k mapping -> apply mapping k) seed tbls
    in
    CCList.fold_left min Int.max_int ints
end

module Part2 = struct
  type interval = {
    low: int;
    high: int;
  }

  let interval_of_pair start length = { low = start; high = start + length - 1 }

  let rec intervals (l : int list) =
    match l with
    | [] -> []
    | x :: y :: l -> interval_of_pair x y :: intervals l
    | _ -> failwith "list does not have even length"
end

module Parse = struct
  let line =
    let open CCParse in
    let* destination_start = U.int <* white in
    let* source_start = U.int <* white in
    let* length = U.int in
    pure { destination_start; source_start; length }

  let mapping : mapping CCParse.t = CCParse.many line

  let map_ : map CCParse.t =
    let open CCParse in
    let* source = U.word <* skip (string "-to-") in
    let* destination = U.word <* skip white <* skip (string "map:") in
    let* mapping = mapping <* skip white in
    pure { source; destination; mapping }

  let seeds : seeds CCParse.t =
    let open CCParse in
    let+ seeds = skip_space *> string "seeds:" *> sep ~by:white U.int in
    seeds

  let almanac : t CCParse.t =
    let open CCParse in
    let* seeds = seeds <* skip white in
    let* maps = many map_ in

    pure { seeds; maps }
end

(* let () =
   match CCParse.parse_file_e Parse.almanac "input.txt" with
   | Ok seeds -> CCFormat.printf "%a" CCFormat.(int) (Part1.ans seeds)
   | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err *)

let () = CCFormat.printf "%a" (CCList.pp CCFormat.(int)) CCList.(79 -- 92)
