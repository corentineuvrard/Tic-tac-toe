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

(*
8 - 4 + 1 = 5

0 1 2 3
1 2 3 4
2 3 4 5
3 4 5 6
4 5 6 7

8 9 10 11
9 10 11 12
10 11 12 13
11 12 13 14
12 13 14 15

...

5 * 8 = 40 lignes
5 * 8 = 40 colonnes

4 13 22 31

3 12 21 30
12 21 30 39

2 11 20 29
11 20 29 38
20 29 38 47

...

1 + 2 + 3 + 4 + 5 + 4 + 3 + 2 + 1 = 25
25 * 2 = 50

total = 130 solutions pour des combinaisons de 4 sur une grille 8x8

lignes:
 1) i % (8 - 1) <= 4 
 2) (i - 1) % (8 - 1) <= 4
 3) (i - 2) % (8 - 1) <= 4
 4) (i - 3) % (8 - 1) <= 4

colonnes:
 1) i 
 2) i + rows
 3) i + 2 * rows 
 4) i + 3 * rows

diagonales:
 1)
 2)
 3)
 4)








 Objectifs:
 - finir la partie si 4 symboles sont alignés
 - faire en sorte que l'IA suive l'algo. minimax

*)
