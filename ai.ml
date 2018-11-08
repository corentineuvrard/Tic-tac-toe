#load "graphics.cma";;

#use "variables.ml";;
#use "drawings.ml";;

(* Evaluate the 4 next turns *)
(*let evaluate() =

;;*)

(*let ai() =
  for i = 1 to (rows * columns - 1) do
    if board.(i) = "X" then
    begin
      if board.(i-1) = " " then
      begin
        draw_circle (((i-1) mod 8)*square_width+50) (((i-1) / 8)*square_height+50);
        board.(i-1) <- "O";
        add_move (i-1);
      end;
    end;
  done;
;;*)
