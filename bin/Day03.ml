open Core
open Advent

let find_max_mod (maxi : int) (bat : int list) : int * int =
  let baselen = List.length bat in
  let rec aux (rest : int list) (compare : int) (i : int) : int * int =
    match rest with
    | [] -> compare, i
    | x :: rem when x > compare && List.length rem > maxi - 1 ->
      aux rem x (baselen - List.length rem - 1)
    | _ :: rem -> aux rem compare i
  in
  aux bat 0 0
;;

let find_bat_pt1 (bat : int list) : int =
  let tens, i = find_max_mod 1 bat in
  let ones, _ = List.drop bat (i + 1) |> find_max_mod 0 in
  (tens * 10) + ones
;;

let find_bat_pt2 (bat : int list) : int =
  let rec aux (rest : int list) (place : int) (acc : int) : int =
    if place < 0
    then acc
    else (
      let x, i = find_max_mod place rest in
      let v = x * Int.pow 10 place in
      aux (List.drop rest (i + 1)) (place - 1) (acc + v))
  in
  aux bat 11 0
;;

let () =
  let batteries =
    read_lines "./inputs/d03/input.txt"
    |> List.map ~f:(fun str ->
      String.to_list str |> List.map ~f:(fun c -> Char.to_int c - 48))
  in
  let res = List.map batteries ~f:find_bat_pt1 |> sumlist in
  let res2 = List.map batteries ~f:find_bat_pt2 |> sumlist in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
