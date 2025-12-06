open Core
open Advent

let eval_problem (prob : string list) : int =
  match List.rev prob with
  | "*" :: nums ->
    List.map nums ~f:int_of_string |> List.fold ~init:1 ~f:(fun acc n -> acc * n)
  | "+" :: nums ->
    List.map nums ~f:int_of_string |> List.fold ~init:0 ~f:(fun acc n -> acc + n)
  | _ -> raise (Invalid_argument "bad range data")
;;

let split_on_empty_list (lst : char list list) : char list list list =
  let rec aux acc hold rest : char list list list =
    match rest with
    | chars :: rest when List.for_all chars ~f:(Char.equal ' ') ->
      aux (hold :: acc) [] rest
    | chars :: rest -> aux acc (chars :: hold) rest
    | _ -> hold :: acc
  in
  aux [] [] lst
;;

let fix_stupid_cephalopods (prob : char list list) : string list =
  let strings = List.map prob ~f:(fun c -> String.of_list c) |> List.rev in
  let top = List.hd_exn strings in
  let sym = String.sub top ~pos:(String.length top - 1) ~len:1 in
  let trimmed = String.sub top ~pos:0 ~len:(String.length top - 1) in
  List.map (sym :: trimmed :: List.tl_exn strings) ~f:String.strip |> List.rev
;;

let () =
  let lines = read_lines "./inputs/d06/bot.txt" in
  let pt1parse =
    List.map lines ~f:(String.split ~on:' ')
    |> List.map ~f:(fun l -> List.filter l ~f:(fun s -> String.length s > 0))
    |> matrix_transpose
  in
  let res = List.map pt1parse ~f:eval_problem |> sumlist in
  let pt2parse =
    List.map lines ~f:String.to_list
    |> matrix_transpose
    |> split_on_empty_list
    |> List.map ~f:fix_stupid_cephalopods
  in
  let res2 = List.map pt2parse ~f:eval_problem |> sumlist in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
