#load "graphics.cma";;
module G = Graphics;;

#use "variables.ml";;
#use "drawings.ml";;
#use "ai.ml";;

(* Define if the game is finished or not *)
let end_of_game =
  !draw_game || !x_wins || !o_wins
;;

(* Get the ID of the square where the click has occured *)
let get_square position_x position_y =
  position_x / square_width + columns * (position_y / square_height)
;;

(* Reset the game *)
let reset() =
  (* Unmark all squares *)
  for i = 0 to (rows * columns - 1) do
    board.(i) <- " ";
  done;
  (* Reset the states of the game *)
  draw_game := false;
  x_wins := false;
  o_wins := false;
;;

(* Main loop *)
let main() =
  (* Create the graphics window passing the size of the window globally defined *)
  (* For Debian OS *)
  (*let graph = " " ^ string_of_int window_size ^ "x" ^ string_of_int window_size in*)
  (* For Windows 8.1 OS *)
  let graph = " " ^ string_of_int (window_size + 19) ^ "x" ^ string_of_int (window_size + 48) in
  G.set_window_title "Morpion";
  G.open_graph graph;
  grid();

  while true do
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
    if e.G.button && not(end_of_game) then
    begin
      if board.(get_square e.G.mouse_x e.G.mouse_y) = " " then
      begin
        draw_cross e.G.mouse_x e.G.mouse_y;
        board.(get_square e.G.mouse_x e.G.mouse_y) <- "X";
        ai();
      end;
    end;
  done
;;

(* Enter the main loop *)
main();;
