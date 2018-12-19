open Variables;;

module G = Graphics;;

(* Draw a grid *)
let grid() =
  (* Erase the graphics window *)
  G.clear_graph();
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      (* Define the color that will be used for the next drawings *)
      G.set_color G.black;
      (* Draw the black lines that compose the grid *)
      G.draw_rect (i * square_width) (j * square_height) (square_width) (square_height);
    done;
  done
;;

(* Draw a cross *)
let draw_cross position_x position_y =
  let first_segment = [| square_width * (position_x / square_width) + square_width / 4,
                         square_height * (position_y / square_height) + square_height / 4,
                         square_width * (position_x / square_width) + square_width - square_width / 4,
                         square_height * (position_y / square_height) + square_height - square_height / 4 |] in
  G.draw_segments first_segment;

  let second_segment = [| square_width * (position_x / square_width) + square_width / 4,
                            square_height * (position_y / square_height) + square_height - square_height / 4,
                            square_width * (position_x / square_width) + square_width - square_width / 4,
                            square_height * (position_y / square_height) + square_height / 4 |] in
  G.draw_segments second_segment
;;

(* Draw a circle *)
let draw_circle position_x position_y =
  let x = square_width * (position_x / square_width) + square_width / 2 in
  let y = square_height * (position_y / square_height) + square_height / 2 in
  G.draw_circle x y 25
;;
