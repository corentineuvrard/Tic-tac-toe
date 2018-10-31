module A = Array;;
module L = List;;

(* Global variables *)

(* Size of the graphics window *)
let window_size = 800;;

(* Number of rows and columns *)
let rows = 8;;
let columns = 8;;

(* Number of squares in the grid *)
let squares = rows * columns;;

(* Size of a square of the grid *)
let square_width = window_size / rows;;
let square_height = window_size / columns;;

(* Define whether a square is marked or not *)
let board = A.make 64 " ";;

(* Update the list of the empty squares *)
let update_empty_squares a =
  (* Run through each square of the board *)
  let rec loop i =
    (* Return an empty list when all squares of the board have been checked *)
    if i >= A.length a then
      []
    else
      (* Check if the square is empty at the index i *)
      let x = a.(i) in
      if x = " " then
        (* Concatenate the current index with the next matching indexes recursively *)
        i::(loop (i+1))
      else
        (* Check the conditions recursively for the next indexes *)
        (loop (i+1))
  in
  (* Start checking from the index 0 *)
  loop 0
;;

(* List of the empty squares *)
let empty_squares =
  update_empty_squares board
;;

(* Last move that has been played in the game *)
let last_move = [];;

(* States of the game *)
let draw_game = ref false;;
let x_wins = ref false;;
let o_wins = ref false;;