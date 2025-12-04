open Core
open Advent

let offsets = [ 0, 1; 1, 1; 1, 0; 1, -1; 0, -1; -1, -1; -1, 0; -1, 1 ]

let check_surround ((ox, oy) : int * int) (map : char array array) : int =
  let height = Array.length map in
  let width = Array.length map.(0) in
  List.count offsets ~f:(fun (dx, dy) ->
    let nx = ox + dx in
    let ny = oy + dy in
    if nx >= 0 && nx < width && ny >= 0 && ny < height
    then Char.equal map.(ny).(nx) '@'
    else false)
;;

let clear_paper (omap : char array array) : char array array =
  let map = Array.copy omap in
  let height = Array.length map in
  let width = Array.length map.(0) in
  let rec aux ((ox, oy) : int * int) (shift : bool) : unit =
    match ox, oy with
    | x, _ when x >= width -> aux (0, oy + 1) shift
    | _, y when y >= height -> if shift then aux (0, 0) false else ()
    | _ ->
      (match map.(oy).(ox) with
       | '@' when check_surround (ox, oy) map < 4 ->
         map.(oy).(ox) <- 'x';
         aux (ox + 1, oy) true
       | _ -> aux (ox + 1, oy) shift)
  in
  aux (0, 0) false;
  map
;;

let () =
  let map = read_lines "./inputs/d04/input.txt" |> construct_char_grid in
  let res =
    Array.foldi map ~init:0 ~f:(fun y acc carr ->
      Array.counti carr ~f:(fun x c -> check_surround (x, y) map < 4 && Char.equal c '@')
      + acc)
  in
  let pt2map = clear_paper map in
  let res2 =
    Array.fold pt2map ~init:0 ~f:(fun acc carr ->
      Array.count carr ~f:(fun c -> Char.equal c 'x') + acc)
  in
  Printf.printf "\nPart 1: %i\nPart 2: %i\n" res res2
;;
