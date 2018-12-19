open Variables;;
open Drawings;;
open Functions;;
open Ai;;

module G = Graphics;;
module L = List;;

(* Define if the game is finished or not *)
let set_eog() =
  end_of_game := !draw_game || !x_wins || !o_wins;
  (* If it is the end of the game *)
  if !end_of_game then
  begin
    (* Display the result on the console *)
    if !x_wins then
    begin
      G.moveto 330 850;
      G.draw_string "Vous avez gagne ! :)";
    end
    else if !o_wins then
    begin
      G.moveto 330 850;
      G.draw_string "Vous avez perdu ! :(";
    end
    else if !draw_game then
    begin
      G.moveto 360 850;
      G.draw_string "Egalite ! :|";
    end;
    G.moveto 250 830;
    G.draw_string "Appuyez sur R pour recommencer ou Q pour quitter";
  end
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
  (* Reset the winner *)
  win := false;
  (* Reset the states of the game *)
  end_of_game := false;
  draw_game := false;
  x_wins := false;
  o_wins := false;
  player_turn := true;
;;

(* Main loop *)
let main() =
  (* Create the graphics window passing the size of the window globally defined *)
  let graph = graphics_debian in
  G.open_graph graph;
  G.set_window_title "Morpion";
  grid();

  while true do
    (* The player has to play or it is the end of the game *)
    if !player_turn || !end_of_game then
    begin
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
	      | 'q' -> G.close_graph(); exit 0;
	      (* Ignore other keys *)
	      | _ -> ();
	    end;
	    (* In case of a button event when the player has to play *)
	    if e.G.button && not(!end_of_game) && !player_turn then
	    begin
	      let x = get_square_x e.G.mouse_x in
	      let y = get_square_y e.G.mouse_y in
	      (* If the square is empty *)
	      if board.(x).(y) = " " then
	      begin
	        (* Play the move *)
	        play_move "X" x y true;
	        (* It is now the AI turn *)
	        player_turn := false;
	        (* Check if the player wins *)
	        if !win then x_wins := true;
	      end
	    end;
      set_eog();
  	end
  	(* The AI has to play *)
  	else if not(!player_turn) && not(!end_of_game) then
  	begin
  		(* Make the AI play *)
  		ai();
		  (* Check if the AI wins *)
      if !win then o_wins := true
      (* Check if the board is full *)
      else if (L.length !moves) = (rows * columns) then draw_game := true;
      set_eog();
  	end;
  done
;;

(* Enter the main loop *)
main();;
