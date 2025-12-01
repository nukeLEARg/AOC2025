open Core
open Advent

let floor_div (x : int) : int = if x >= 0 then x / 100 else (x - 99) / 100

let count_zeros (turns : int list) : int =
  let rec aux (zcount : int) (pos : int) (rturns : int list) : int =
    match rturns with
    | [] -> zcount
    | x :: rest ->
      let npos = (pos + x) % 100 in
      if npos < 0
      then aux zcount (npos + 100) rest
      else if npos = 0
      then aux (zcount + 1) npos rest
      else aux zcount npos rest
  in
  aux 0 50 turns
;;

let count_zeros2 (turns : int list) : int =
  let rec aux (zcount : int) (pos : int) (rturns : int list) : int =
    match rturns with
    | [] -> zcount
    | x :: rest ->
      let npos = pos + x in
      let loops =
        if x > 0
        then floor_div npos - floor_div pos
        else floor_div (pos - 1) - floor_div (npos - 1)
      in
      aux (zcount + loops) npos rest
  in
  aux 0 50 turns
;;

let () =
  let turns =
    read_lines "./inputs/d01/input.txt"
    |> List.map ~f:(fun str ->
      let sstr = String.sub str ~pos:1 ~len:(String.length str - 1) |> int_of_string in
      if Char.equal (String.get str 0) 'L' then sstr * -1 else sstr)
  in
  let res = count_zeros turns in
  let res2 = count_zeros2 turns in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
