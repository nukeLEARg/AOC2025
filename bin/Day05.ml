open Core
open Advent

let build_range (str : string) : int * int =
  match String.split ~on:'-' str |> List.map ~f:int_of_string with
  | [ x; y ] when y > x -> x, y
  | [ x; y ] -> y, x
  | _ -> raise (Invalid_argument "bad range data")
;;

let merge_ranges (sorted_ranges : (int * int) list) : (int * int) list =
  let rec aux (acc : (int * int) list) (rest : (int * int) list) : (int * int) list =
    match rest with
    (*the full overlap case*)
    | (mina, maxa) :: (_, maxb) :: rest when maxa >= maxb ->
      let merged = mina, maxa in
      aux acc (merged :: rest)
    (*the merge case*)
    | (mina, maxa) :: (minb, maxb) :: rest when minb <= maxa ->
      let merged = mina, maxb in
      aux acc (merged :: rest)
    (*no overlap*)
    | range :: rest -> aux (range :: acc) rest
    | _ -> acc
  in
  aux [] sorted_ranges
;;

let pt2 (ranges : (int * int) list) : int =
  let sorted_ranges =
    List.sort ranges ~compare:(fun (min1, _) (min2, _) -> Int.compare min1 min2)
  in
  let merged = merge_ranges sorted_ranges in
  List.fold merged ~init:0 ~f:(fun acc (min, max) -> acc + max - min + 1)
;;

let () =
  let ranges, ids = read_lines "./inputs/d05/input.txt" |> split_on_empty [] in
  let ranges = List.map ranges ~f:build_range in
  let ids = List.map ids ~f:int_of_string in
  let res =
    List.count ids ~f:(fun id ->
      List.exists ranges ~f:(fun (min, max) -> id >= min && id <= max))
  in
  let res2 = pt2 ranges in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
