#load "graphics.cma";;
module G = Graphics;;

#use "variables.ml";;
#use "drawings.ml";;
#use "ai.ml";;

(* Define if the game is finished or not *)
let set_eog() =
  end_of_game := !draw_game || !x_wins || !o_wins;
;;

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
    if marks.(3) != -1 then
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
    if marks.(3) != -1 then
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
(*let check_diagonal index symbol =
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
  let rec loop i j =
    (* Add the x and y coordinates of the current square to the array marks *)
    marks.(!counter).(0) <- i;
    marks.(!counter).(1) <- j;
    (* There is a winning combination if the last element of marks is not equals to -1 *)
    if marks.(3) != [|-1;-1|] then
      winning_diagonal := true
    else
    begin
      (* If the current square is a square on the left side *)
      if i <= 2 then
      begin
        (* If the current square is on the bottom left corner *)
        if i + j <= 2 then
        begin
          (* If there is the same symbol on the top right side square and that square has not been checked *)
          if board.(i+1).(j+1) = symbol && not(get_marks_diags marks (i+1) (j+1)) then
          (* Increment the counter and check the square on the top right side of the current square *)
          begin
            counter := !counter + 1;
            loop (i+1) (j+1);
          end
        end
        (* If the current square is on the top left corner *)
        else if j >= 5 then
        begin
          (* If there is the same symbol on the bottom right side square and that square has not been checked *)
          if board.(i+1).(j-1) = symbol && not(get_marks_diags marks (i+1) (j-1)) then
          (* Increment the counter and check the square on the bottom right side of the current square *)
          begin
            counter := !counter + 1;
            loop (i+1) (j-1);
          end
        end
      end
      (* If the current square is a square on the right edge *)
      else if i = 7 then
      begin
        (* If the current square is on the bottom right corner *)
        if j = 0 then
        begin
          (* If there is the same symbol on the top left side square and that square has not been checked *)
          if board.(i-1).(j+1) = symbol && not(get_marks_diags marks (i-1) (j+1)) then
          (* Increment the counter and check the square on the top left side of the current square *)
          begin
            counter := !counter + 1;
            loop (i-1) (j+1);
          end
        end
        (* If the current square is on the top right corner *)
        if j = 7 then
        begin
          (* If there is the same symbol on the bottom left side square and that square has not been checked *)
          if board.(i-1).(j-1) = symbol && not(get_marks_diags marks (i-1) (j-1)) then
          (* Increment the counter and check the square on the bottom left side of the current square *)
          begin
            counter := !counter + 1;
            loop (i-1) (j-1)
          end
        end
      end
      (* If the current square is not on any edges *)
      else
      begin
        (* Check the bottom side of the current square *)
        if board.(x).(j-1) = symbol && not(get_marks marks (j-1)) then
        begin
          counter := !counter + 1;
          loop (j-1);
        end;
        (* Check the top side of the current square *)
        if marks.(3) = -1 && board.(x).(j+1) = symbol && not(get_marks marks (j+1)) then
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
;;*)

(* Define if there is a winner or not *)
let set_winner() =
  (* If there are enough moves in the game in order to get a winning combination *)
  if L.length !moves > 6 then
  begin
    (* Get the index of the square where the last move has been played *)
    let index = (String.make 1 (List.hd !moves).[1]) ^ (String.make 1 (List.hd !moves).[2]) in
    (* Get the last symbol that has been put on the grid *)
    let symbol = String.make 1 (List.hd !moves).[0] in
    (* If there is a winning  combination *)
    if check_rows index symbol then
    begin
      (* The last player to play wins *)
      if symbol = "X" then
        x_wins := true
      else if symbol = "O" then
        o_wins := true
    end
    else if check_columns index symbol then
    begin
      (* The last player to play wins *)
      if symbol = "X" then
        x_wins := true
      else if symbol = "O" then
        o_wins := true
    end
    (* If the grid is full and no winner then it is a draw *)
    else if L.length !moves = rows * columns - 1 then
      draw_game := true
  end
  else
    ()
;;

(* Get the x coordinate of the square where the click has occured *)
let get_square_x position_x =
  position_x / square_width
;;

(* Get the y coordinate of the square where the click has occured *)
let get_square_y position_y =
  position_y / square_height
;;

(* Reset the game *)
let reset() =
  (* Unmark all squares *)
  for i = 0 to (rows - 1) do
    for j = 0 to (columns - 1) do
      board.(i).(j) <- " ";
    done;
  done;
  (* Reset the number of moves *)
  moves := [];
  (* Reset the states of the game *)
  draw_game := false;
  x_wins := false;
  o_wins := false;
;;

(* Main loop *)
let main() =
  (* Create the graphics window passing the size of the window globally defined *)
  (* For Debian OS *)
  let graph = " " ^ string_of_int window_size ^ "x" ^ string_of_int window_size in
  (* For Windows 8.1 OS *)
  (*let graph = " " ^ string_of_int (window_size + 19) ^ "x" ^ string_of_int (window_size + 48) in*)
  G.set_window_title "Morpion";
  G.open_graph graph;
  grid();

  while true do
    set_eog();
    (* Catch key pressed and button events *)
    let e = G.wait_next_event[G.Key_pressed; G.Button_down] in
    (* In case of a key pressed event *)
    if e.G.keypressed then
    begin
      (* Check the key that has been pressed *)
      match e.G.key with
      (* Reset the grid *)
      | 'r' -> reset(); grid();
      (* Close the graphics window *)
      | 'q' -> G.close_graph(); (* exit 0 *)
      (* Ignore other keys *)
      | _ -> ();
    end;
    (* In case of a button event *)
    if e.G.button && not(!end_of_game) then
    begin
      let x = get_square_x e.G.mouse_x in
      let y = get_square_y e.G.mouse_y in
      (* If the square is empty *)
      if board.(x).(y) = " " then
      begin
        (* Draw a cross *)
        draw_cross e.G.mouse_x e.G.mouse_y;
        (* Add the cross to the board array *)
        board.(x).(y) <- "X";
        (* Add the move to the array of moves *)
        add_move ("X" ^ (string_of_int x) ^ (string_of_int y));
        (* Check if the move makes the player win *)
        set_winner();
        (* Let the AI play *)
        (*ai();*)
      end;
    end;
  done
;;

(* Enter the main loop *)
main();;
