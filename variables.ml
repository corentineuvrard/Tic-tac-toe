module A = Array;;

(* Global variables *)

(* Size of the graphics window *)
let window_size = 800;;

(* Graphic for Debian OS *)
let graphics_debian = " " ^ string_of_int window_size ^ "x" ^ string_of_int (window_size + 100);;

(* Graphic for Windows 8.1 OS *)
let graphics_windows = " " ^ string_of_int (window_size + 19) ^ "x" ^ string_of_int (window_size + 148);;

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

(* Define who has to play the next move *)
let player_turn = ref true;;

(* Define if there is a winner *)
let win = ref false;;

(* States of the game *)
let draw_game = ref false;;
let x_wins = ref false;;
let o_wins = ref false;;
let end_of_game = ref false;;

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

(* List of moves that have been played in the game *)
let moves : string list ref = ref [];;

(* List of the empty squares of the board *)
let empty_squares = ref [];;

let update_empty_squares() =
	(* Initialize the list *)
	empty_squares := [];
	(* Run through each square of the board *)
	for i = 0 to (rows - 1) do
		for j = 0 to (columns - 1) do
			if board.(i).(j) = " " then
			begin
				(* Coordinates of the empty square *)
				let coordinates = string_of_int i ^ string_of_int j in
				(* Add the coordinates of the empty square to the list of empty squares *)
				empty_squares := !empty_squares @ [coordinates];
			end;
		done;
	done;
;;
