(* type cubes =
     | Red of int
     | Green of int
     | Blue of int
   [@@deriving show { with_path = false }] *)

type cubes = {
  red: int option;
  green: int option;
  blue: int option;
}
[@@deriving show { with_path = false }]

(* type grab = cubes list [@@deriving show { with_path = false }] *)

type game = {
  id: int;
  grabs: cubes list;
}
[@@deriving show { with_path = false }]

type record = game list [@@deriving show { with_path = false }]
