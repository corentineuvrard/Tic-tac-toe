open Variables;;
open Drawings;;

module C = Char;;
module L = List;;

(* Global functions *)

(* Get the x coordinate of the square where the click has occured *)
let get_square_x position_x = position_x / square_width;;

(* Get the y coordinate of the square where the click has occured *)
let get_square_y position_y = position_y / square_height;;

(* Check if a square has already been checked for rows and columns *)
let get_marks_rows_cols a e =
  (* By default it returns false *)
  let is_marked = ref false in
  let rec loop i =
    (* Checking trhough the array of the marked coordinates *)
    if i < Array.length a then
    begin
      (* Return true if the element matched with an element of the array *)
      if a.(i) = e then
        is_marked := true
      (* Check with the next elements of the array *)
      else
        loop (i+1)
    end
  in
  loop 0;
  (* Return if the coordinate has been marked *)
  !is_marked
;;

(* Check if a square has already been checked for diagonals *)
let get_marks_diags a x y =
  (* By default it returns false *)
  let is_marked = ref false in
  let rec loop i =
    (* Checking trhough the array of the marked coordinates *)
    if i < Array.length a then
    begin
      (* Return true if the element matched with an element of the array *)
      if a.(i).(0) = x && a.(i).(1) = y then
        is_marked := true
      (* Check with the next elements of the array *)
      else
        loop (i+1)
    end
  in
  loop 0;
  (* Return if the coordinates has been marked *)
  !is_marked
;;

(* Check if there is a winning pattern on a row *)
let check_rows index symbol =
  (* By default there is no winning pattern *)
  let winning_row = ref false in
  (* Get the x and y coordinates from the index string *)
  (* int_of_char returns the ASCII code of the char, so it is necessary to substract 48 to it to get the corresponding int *)
  let x = int_of_char index.[0] - 48 in
  let y = int_of_char index.[1] - 48 in
  (* -1 means that there is no x coordinate marked *)
  let marks = [|-1;-1;-1;-1|] in
  (* Count the number of squares checked *)
  let counter = ref 0 in
  (* Check the squares *)
  let rec loop i =
    (* Add the x coordinate of the current square to the array marks *)
    marks.(!counter) <- i;
    (* There is a winning combination if the last element of marks is not equals to -1 *)
    if marks.(3) <> -1 then
      winning_row := true
    else
    begin
      (* If the current square is the square on the left edge *)
      if i = 0 then
      begin
        (* If there is the same symbol on the right side square and that square has not been checked *)
        if board.(i+1).(y) = symbol && not(get_marks_rows_cols marks (i+1)) then
        (* Increment the counter and check the square on the right side of the current square *)
        begin
          counter := !counter + 1;
          loop (i+1);
        end
      end
      (* If the current square is the square on the right edge *)
      else if i = 7 then
      begin
        (* If there is the same symbol on the left side square and that square has not been checked *)
        if board.(i-1).(y) = symbol && not(get_marks_rows_cols marks (i-1)) then
        (* Increment the counter and check the square on the left side of the current square *)
        begin
          counter := !counter + 1;
          loop (i-1);
        end
      end
      (* If the current square is not on any edges *)
      else
      begin
        (* Check the left side of the current square *)
        if board.(i-1).(y) = symbol && not(get_marks_rows_cols marks (i-1)) then
        begin
          counter := !counter + 1;
          loop (i-1);
        end;
        (* Check the right side of the current square *)
        if marks.(3) = -1 && board.(i+1).(y) = symbol && not(get_marks_rows_cols marks (i+1)) then
        begin
          counter := !counter + 1;
          loop (i+1);
        end
      end
    end
  in
  loop x;
  (* Return if there is a winning row or not *)
  !winning_row
;;

(* Check if there is a winning pattern on a column *)
let check_columns index symbol =
  (* By default there is no winning pattern *)
  let winning_column = ref false in
  (* Get the x and y coordinates from the index string *)
  (* int_of_char returns the ASCII code of the char, so it is necessary to substract 48 to it to get the corresponding int *)
  let x = int_of_char index.[0] - 48 in
  let y = int_of_char index.[1] - 48 in
  (* -1 means that there is no y coordinate marked *)
  let marks = [|-1;-1;-1;-1|] in
  (* Count the number of squares checked *)
  let counter = ref 0 in
  (* Check the squares *)
  let rec loop j =
    (* Add the y coordinate of the current square to the array marks *)
    marks.(!counter) <- j;
    (* There is a winning combination if the last element of marks is not equals to -1 *)
    if marks.(3) <> -1 then
      winning_column := true
    else
    begin
      (* If the current square is the square on the bottom edge *)
      if j = 0 then
      begin
        (* If there is the same symbol on the top side square and that square has not been checked *)
        if board.(x).(j+1) = symbol && not(get_marks_rows_cols marks (j+1)) then
        (* Increment the counter and check the square on the top side of the current square *)
        begin
          counter := !counter + 1;
          loop (j+1);
        end
      end
      (* If the current square is the square on the top edge *)
      else if j = 7 then
      begin
        (* If there is the same symbol on the bottom side square and that square has not been checked *)
        if board.(x).(j-1) = symbol && not(get_marks_rows_cols marks (j-1)) then
        (* Increment the counter and check the square on the bottom side of the current square *)
        begin
          counter := !counter + 1;
          loop (j-1);
        end
      end
      (* If the current square is not on any edges *)
      else
      begin
        (* Check the bottom side of the current square *)
        if board.(x).(j-1) = symbol && not(get_marks_rows_cols marks (j-1)) then
        begin
          counter := !counter + 1;
          loop (j-1);
        end;
        (* Check the top side of the current square *)
        if marks.(3) = -1 && board.(x).(j+1) = symbol && not(get_marks_rows_cols marks (j+1)) then
        begin
          counter := !counter + 1;
          loop (j+1);
        end
      end
    end
  in
  loop y;
  (* Return if there is a winning column or not *)
  !winning_column
;;

(* Check if there is a winning pattern on a diagonal *)
let check_diagonals index symbol =
  (* By default there is no winning pattern *)
  let winning_diagonal = ref false in
  (* Get the x and y coordinates from the index string *)
  (* int_of_char returns the ASCII code of the char, so it is necessary to substract 48 to it to get the corresponding int *)
  let x = int_of_char index.[0] - 48 in
  let y = int_of_char index.[1] - 48 in
  (* -1 means that there is no x and y coordinates marked *)
  let marks = [|[|-1;-1|];
                [|-1;-1|];
                [|-1;-1|];
                [|-1;-1|]|] in
  (* Count the number of squares checked *)
  let counter = ref 0 in
  (* Check the squares *)
  let rec loop i j diag_type =
    (* Add the x and y coordinates of the current square to the array marks *)
    marks.(!counter).(0) <- i;
    marks.(!counter).(1) <- j;
    (* There is a winning combination if the last element of marks is not equals to [|-1;-1|] *)
    if marks.(3).(0) <> -1 && marks.(3).(1) <> -1 then
      winning_diagonal := true
    (* Check the diagonals matrix to know what kind of diagonal combination there can be on a given square *)
    else
    begin
      (* For the type of diagonals going from the bottom left corner to the top right corner *)
      if diags_matrix.(i).(j) = "/" then
      begin
        (* If the square is on the bottom left corner *)
        if i = 0 || j = 0 then
        begin
          (* If there is the same symbol on the top right side square and that square has not been marked *)
          if board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
          (* Increment the counter and check the square on the top right side of the current square *)
          begin
            counter := !counter + 1;
            loop (i+1) (j+1) "/";
          end
        end
        (* If the square is on the top right corner *)
        else if i = (Array.length board) - 1 || j = (Array.length board.(0)) - 1 then
        begin
          (* If there is the same symbol on the bottom left side square and that square has not been marked *)
          if board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
          (* Increment the counter and check the square on the bottom left side of the current square *)
          begin
            counter := !counter + 1;
            loop (i-1) (j-1) "/"
          end
        end
        (* If the square is not on any edges *)
        else
        begin
          (* Check the bottom left side of the current square *)
          if board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
          begin
            counter := !counter + 1;
            loop (i-1) (j-1) "/";
          end;
          (* Check the top right side of the current square *)
          if marks.(3) = [|-1;-1|] && board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
          begin
            counter := !counter + 1;
            loop (i+1) (j+1) "/";
          end
        end
      end
      (* For the type of diagonals going from the top left corner to the bottom right corner *)
      else if diags_matrix.(i).(j) = "\\" then
      begin
        (* If the square is on the top left corner *)
        if i = 0 || j = (Array.length board.(0)) - 1 then
        begin
          (* If there is the same symbol on the bottom right side square and that square has not been checked *)
          if board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
          (* Increment the counter and check the square on the bottom right side of the current square *)
          begin
            counter := !counter + 1;
            loop (i+1) (j-1) "\\";
          end
        end
        (* If the square is on the bottom right corner *)
        else if i = (Array.length board) - 1 || j = 0 then
        begin
          (* If there is the same symbol on the top left side square and that square has not been marked *)
          if board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
          (* Increment the counter and check the square on the top left side of the current square *)
          begin
            counter := !counter + 1;
            loop (i-1) (j+1) "\\";
          end
        end
        (* If the square is not on any edges *)
        else
        begin
          (* Check the top left side of the current square *)
          if board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
          begin
            counter := !counter + 1;
            loop (i-1) (j+1) "\\";
          end;
          (* Check the bottom right side of the current square *)
          if marks.(3) = [|-1;-1|] && board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
          begin
            counter := !counter + 1;
            loop (i+1) (j-1) "\\";
          end
        end
      end
      (* For any types of diagonals *)
      else if diags_matrix.(i).(j) = "X" then
      begin
        (* If the square is on the left edge *)
        if i = 0 then
        begin
          (* Check the top right side of the current square *)
          if diag_type <> "\\" && board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i+1) (j+1) "/"
            else
              loop (i+1) (j+1) diag_type
          end;
          (* Check the bottom right side of the current square *)
          if diag_type <> "/" && marks.(3) = [|-1;-1|] && board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i+1) (j-1) "\\"
            else
              loop (i+1) (j-1) diag_type
          end
        end
        (* If the square is on the bottom edge *)
        else if j = 0 then
        begin
          (* Check the top left side of the current square *)
          if diag_type <> "/" && board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i-1) (j+1) "\\"
            else
              loop (i-1) (j+1) diag_type
          end;
          (* Check the top right side of the current square *)
          if diag_type <> "\\" && marks.(3) = [|-1;-1|] && board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i+1) (j+1) "/"
            else
              loop (i+1) (j+1) diag_type
          end
        end
        (* If the square is on the right edge *)
        else if i = (Array.length board) - 1 then
        begin
          (* Check the top left side of the current square *)
          if diag_type <> "/" && board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i-1) (j+1) "\\"
            else
              loop (i-1) (j+1) diag_type
          end;
          (* Check the bottom left side of the current square *)
          if diag_type <> "\\" && marks.(3) = [|-1;-1|] && board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i-1) (j-1) "/"
            else
              loop (i-1) (j-1) diag_type
          end
        end
        (* If the square is on the top edge *)
        else if j = (Array.length board.(0)) - 1 then
        begin
          (* Check the bottom left side of the current square *)
          if diag_type <> "\\" && board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i-1) (j-1) "/"
            else
              loop (i-1) (j-1) diag_type
          end;
          (* Check the bottom right side of the current square *)
          if diag_type <> "/" && marks.(3) = [|-1;-1|] && board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
          begin
            counter := !counter + 1;
            if diag_type = " " then
              loop (i+1) (j-1) "\\"
            else
              loop (i+1) (j-1) diag_type
          end
        end
        else
        begin
          if diag_type <> "\\" && (board.(i-1).(j-1) = symbol || board.(i+1).(j+1) = symbol) then
          begin
            (* Reset the counter if it is the first level of recursion *)
            if diag_type = " " then counter := 0;
            (* Check the bottom left side of the current square *)
            if board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
            begin
              counter := !counter + 1;
              if diag_type = " " then
                loop (i-1) (j-1) "/"
              else
                loop (i-1) (j-1) diag_type
            end;
            (* Check the top right side of the current square *)
            if marks.(3) = [|-1;-1|] && board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
            begin
              counter := !counter + 1;
              if diag_type = " " then
                loop (i+1) (j+1) "/"
              else
                loop (i+1) (j+1) diag_type
            end;
          end;
          if diag_type <> "/" && (board.(i+1).(j-1) = symbol || board.(i-1).(j+1) = symbol) then
          begin
            (* Reset the counter if it is the first level of recursion *)
            if diag_type = " " then counter := 0;
            (* Check the bottom right side of the current square *)
            if marks.(3) = [|-1;-1|] && board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
            begin
              counter := !counter + 1;
              if diag_type = " " then
                loop (i+1) (j-1) "\\"
              else
                loop (i+1) (j-1) diag_type
            end;
            (* Check the top left side of the current square *)
            if marks.(3) = [|-1;-1|] && board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
            begin
              counter := !counter + 1;
              if diag_type = " " then
                loop (i-1) (j+1) "\\"
              else
                loop (i-1) (j+1) diag_type
            end
          end
        end
      end
    end
  in
  loop x y " ";
  (* Return if there is a winning column or not *)
  !winning_diagonal
;;

(* Return true if the last player to play wins *)
let has_won() =
	match !moves with
		| [] -> win := false
		| h::t ->
			(* Get the symbol of the last move *)
			let symbol = String.make 1 h.[0] in
			(* Get the index of the last move *)
			let index = (String.make 1 h.[1] ^ (String.make 1 h.[2])) in
			(* Check if it is a win *)
			win := ((check_rows index symbol) || (check_columns index symbol) || (check_diagonals index symbol));
;;

(* Add the last move to the list of moves *)
let add_move move =
  moves := [move] @ !moves;
  ()
;;

(* Play a move *)
let play_move symbol x y real_move =
	(* Add the symbol to the board array *)
	board.(x).(y) <- symbol;
	(* Add the move to the list of moves *)
	add_move (symbol ^ (string_of_int x) ^ (string_of_int y));
	(* Update the list of empty squares *)
	update_empty_squares();
	(* Real move played *)
	if real_move then
	begin
		(* Draw the symbol on the corresponding square of the board *)
		if symbol = "X" then
		begin
			(* Draw a cross *)
	        draw_cross (square_width * x) (square_height * y)
	    end
	    else if symbol = "O" then
	    begin
	    	(* Draw a circle *)
	        draw_circle (square_width * x) (square_height * y)
	    end;
	    has_won();
	end
;;

(* Remove the last move played *)
let remove_last_move() =
	(* Get the coordinates of the last move *)
	let move_x = ((C.code (L.nth !moves 0).[1]) - 48) in
	let move_y = ((C.code (L.nth !moves 0).[2]) - 48) in
	(* Remove the move from the board *)
	board.(move_x).(move_y) <- " ";
	(* Remove the move from the list of moves *)
	moves := L.tl !moves;
;;
