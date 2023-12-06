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

module Almanac = struct
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
      let* seeds = skip_space *> string "seeds:" *> sep ~by:white U.int in
      pure @@ seeds

    let almanac : t CCParse.t =
      let open CCParse in
      let* seeds = seeds <* skip white in
      let* maps = many map_ in

      pure { seeds; maps }
  end
end

let () =
  match CCParse.parse_string_e Almanac.Parse.almanac test with
  | Ok seeds -> CCFormat.printf "%a" Almanac.pp seeds
  | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err
