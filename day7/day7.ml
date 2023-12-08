[@@@warning "-32"]

let test = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

module Char = struct
  let char_frequency str =
    let freq_table = Hashtbl.create 10 in
    String.iter
      (fun c ->
        let count = try Hashtbl.find freq_table c with Not_found -> 0 in
        Hashtbl.replace freq_table c (count + 1))
      str;
    freq_table
end

module Card = struct
  (** Cards list. Access the index using CCString.index *)
  let cards = CCString.rev "AKQJT98765432"

  let get_index c = CCString.index cards c
end

module Hand = struct
  type t = {
    c1: int;
    c2: int;
    c3: int;
    c4: int;
    c5: int;
  }
  [@@deriving ord, show]

  let of_string s =
    let open Card in
    {
      c1 = get_index s.[0];
      c2 = get_index s.[1];
      c3 = get_index s.[2];
      c4 = get_index s.[3];
      c5 = get_index s.[4];
    }

  module Type = struct
    type t =
      | High_card
      | One_pair
      | Two_pair
      | Three_of_a_kind
      | Full_house
      | Four_of_a_kind
      | Five_of_a_kind
    [@@deriving show, enum]

    let of_string hand =
      let values = Char.char_frequency hand |> CCHashtbl.values_list in

      match CCList.sort Stdlib.compare values with
      | [ 5 ] -> Five_of_a_kind
      | [ 1; 4 ] -> Four_of_a_kind
      | [ 2; 3 ] -> Full_house
      | [ 1; 1; 3 ] -> Three_of_a_kind
      | [ 1; 2; 2 ] -> Two_pair
      | [ 1; 1; 1; 2 ] -> One_pair
      | [ 1; 1; 1; 1; 1 ] -> High_card
      | _ -> failwith "bad hand!"
  end

  let compare (h1 : string) (h2 : string) =
    let cmp =
      Stdlib.compare
        (h1 |> Type.of_string |> Type.to_enum)
        (h2 |> Type.of_string |> Type.to_enum)
    in
    if cmp = 0 then
      compare (h1 |> of_string) (h2 |> of_string)
    else
      cmp
end

type t = pair list

and pair = {
  hand: string;
  bid: int;
}
[@@deriving show]

module Part1 = struct
  let ans (parsed_input : pair list) =
    let sorted =
      CCList.sort (fun p1 p2 -> Hand.compare p1.hand p2.hand) parsed_input
    in
    CCFormat.printf "@.Sorted : %a@." CCFormat.Dump.(list pp_pair) sorted;
    CCList.fold_left ( + ) 0 (CCList.mapi (fun i p -> (i + 1) * p.bid) sorted)
end

module Parse = struct
  let line_ =
    let open CCParse in
    let* l = lookahead all in
    if Slice.is_empty l then
      return None
    else
      let* hand = take 5 <* skip_white in
      let+ bid = U.int in
      Some { hand = hand |> Slice.to_string; bid }

  let input : t CCParse.t =
    let open CCParse in
    each_line line_ >|= CCList.keep_some
end

let () =
  match CCParse.parse_file_e Parse.input "input.txt" with
  | Ok parsed -> CCFormat.printf "@.Answer : %d" (Part1.ans parsed)
  | Error err -> failwith @@ CCFormat.sprintf "failed : %a" CCParse.Error.pp err
