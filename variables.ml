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
let board = A.make_matrix rows columns " ";;

(* Update the list of the empty squares *)
let update_empty_squares a =
  (* Run through each square of the board *)
  let rec loop i j =
      (* Return an empty list when all squares of the board have been checked *)
      if i >= A.length a then
        []
      else if j >= A.length a.(0) then
        loop (i+1) 0
      else
        (* Check if the square is empty at the given index *)
        let symbol = a.(i).(j) in
        if symbol = " " then
          (* Concatenate the current index with the next matching indexes recursively *)
          let element = (string_of_int i) ^ (string_of_int j) in
          element::(loop i (j+1))
        else
          (* Check the conditions recursively for the next indexes *)
          loop i (j+1)
  in
  (* Start checking from the index 0 0 *)
  loop 0 0
;;

(* List of the empty squares *)
let empty_squares =
  update_empty_squares board
;;

(* Array of moves that have been played in the game *)
let moves = ref [];;

(* Add the last move to the array of moves *)
let add_move move =
  moves := [move] @ !moves;
  ()
;;

(* Array that define the type of diagonals on the board *)
let diags_matrix =
  (* Create a matrix filled with spaces *)
  let grid = Array.make_matrix 8 8 " " in
  let rec loop i j =
    if i < Array.length grid then
    begin
      if j < Array.length grid.(0) then
      begin
        (* Set the possible diagonals for the bottom left and the top right corners of the board *)
        if (i+j) <= 2 || ((Array.length grid) - 1 - i + (Array.length grid.(0)) - 1 - j) <= 2 then
        begin
          grid.(i).(j) <- "/";
          loop i (j+1)
        end
        (* Set the possible diagonals for the bottom right and the top left corners of the board *)
        else if ((Array.length grid) - 1 - i + j) <= 2 || (i + (Array.length grid.(0)) - 1 - j) <= 2 then
        begin
          grid.(i).(j) <- "\\";
          loop i (j+1)
        end
        (* Set the possible diagonals for the other squares of the board *)
        else
        begin
          grid.(i).(j) <- "X";
          loop i (j+1)
        end
      end
      else
        loop (i+1) 0
    end
  in
  (* Start the loop from the bottom left square of the grid *)
  loop 0 0;
  (* Return the grid initialized *)
  grid
;;

(* States of the game *)
let draw_game = ref false;;
let x_wins = ref false;;
let o_wins = ref false;;
let end_of_game = ref false;;