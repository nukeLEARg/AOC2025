open Core
open Advent

let pt1check (id : int) : bool =
  let s = string_of_int id in
  let len = String.length s in
  if len % 2 <> 0
  then false
  else (
    let half_len = len / 2 in
    let first_half = String.sub s 0 half_len in
    let second_half = String.sub s half_len half_len in
    String.equal first_half second_half)
;;

let pt2check (id : int) : bool =
  let s = string_of_int id in
  let idlen = String.length s in
  let rec test_lengths (plen : int) : bool =
    if plen > idlen / 2
    then false
    else if idlen % plen = 0
    then (
      let rec aux (i : int) : bool =
        if i >= idlen
        then true
        else if not (Char.equal s.[i] s.[i - plen])
        then false
        else aux (i + 1)
      in
      if aux plen then true else test_lengths (plen + 1))
    else test_lengths (plen + 1)
  in
  test_lengths 1
;;

let find_invalid_ids ((min, max) : int * int) (check : int -> bool) : int list =
  let rec aux (id : int) (acc : int list) : int list =
    if id > max
    then List.rev acc
    else if check id
    then aux (id + 1) (id :: acc)
    else aux (id + 1) acc
  in
  aux min []
;;

let () =
  let ids =
    read_lines "./inputs/d02/bot.txt"
    |> List.hd_exn
    |> String.split ~on:','
    |> List.map ~f:(fun str ->
      let pair = String.split ~on:'-' str |> List.map ~f:int_of_string in
      List.hd_exn pair, List.nth_exn pair 1)
  in
  let part1ids =
    List.map ids ~f:(fun ids -> find_invalid_ids ids pt1check) |> List.concat
  in
  let part2ids =
    List.map ids ~f:(fun ids -> find_invalid_ids ids pt2check) |> List.concat
  in
  let res = sumlist part1ids in
  let res2 = sumlist part2ids in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
