type cubes = {
  red: int option;
  green: int option;
  blue: int option;
}
[@@deriving show { with_path = false }]

type game = {
  id: int;
  grabs: cubes list;
}
[@@deriving show { with_path = false }]

type record = game list [@@deriving show { with_path = false }]
