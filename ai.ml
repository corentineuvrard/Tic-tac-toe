open Variables;;
open Functions;;

module C = Char;;
module L = List;;

(* Calculate the score for a set of 4 neighboring squares *)
let calculate_score_set number_of_symbols symbol opponent =
	let score = ref 0 in
	if not(opponent) then
	begin
		(* 1 point for 1 symbol in the set *)
		if number_of_symbols = 1 then
			score := 1
		(* 10 points for 2 symbols in the set *)
		else if number_of_symbols = 2 then
			score := 10
		(* 100 points for 3 symbols in the set *)
		else if number_of_symbols = 3 then
			score := 100
		(* 1000 points for 4 symbols in the set *)
		else if number_of_symbols = 4 then
			score := 1000
	end;
	(* Return the calculated score *)
	if symbol = "O" then !score
	else (-(!score))
;;

(* Evaluate a line of the board *)
let evaluate_line line_type id_line symbol opponent_symbol =
	let score = ref 0 in
	(* Evaluate a row *)
	if line_type = "-" then
	begin
		(* Check each possible set of 4 neighboring squares in the row*)
		for j1 = 0 to 4 do
			if board.(id_line).(j1) != opponent_symbol then
			begin
				(* Number of same symbols in a set of 4 neighboring squares in the row *)
				let count = ref 0 in
				(* Determine if there is at least one opponent symbol in the set of 4 squares *)
				let opponent = ref false in
				for j2 = j1 to (j1 + 3) do
					if board.(id_line).(j2) = symbol then
						count := !count + 1
					else if board.(id_line).(j2) = opponent_symbol then
						opponent := true;
				done;
				(* Calculate the score *)
				score := !score + calculate_score_set !count symbol !opponent;
			end
		done;
	end
	(* Evaluate a column *)
	else if line_type = "|" then
	begin
		(* Check each possible set of 4 neighboring squares in the column *)
		for i1 = 0 to 4 do
			if board.(i1).(id_line) != opponent_symbol then
			begin
				(* Number of same symbols in a set of 4 neighboring squares in the column *)
				let count = ref 0 in
				(* Determine if there is at least one opponent symbol in the set of 4 squares *)
				let opponent = ref false in
				for i2 = i1 to (i1 + 3) do
					if board.(i2).(id_line) = symbol then
						count := !count + 1
					else if board.(i2).(id_line) = opponent_symbol then
						opponent := true
				done;
				(* Calculate the score *)
				score := !score + calculate_score_set !count symbol !opponent;
			end
		done;
	end
	(* Evaluate a diagonal *)
	else if line_type = "/" then
	begin
		(* Check each possible set of 4 neighboring squares in the diagonal *)
		(* First half of the board *)
		if id_line >= 0 then
		begin
			for d1 = 0 to (4 - id_line) do
				if board.(d1).(d1 + id_line) != opponent_symbol then
				begin
					(* Number of same symbols in a set of 4 neighboring squares in the diagonal *)
					let count = ref 0 in
					(* Determine if there is at least one opponent symbol in the set of 4 squares *)
					let opponent = ref false in
					for d2 = d1 to (d1 + 3) do
						if board.(d2).(d2 + id_line) = symbol then
							count := !count + 1
						else if board.(d2).(d2 + id_line) = opponent_symbol then
							opponent := true
					done;
					(* Calculate the score *)
					score := !score + calculate_score_set !count symbol !opponent;
				end
			done;
		end
		(* Second half of the board *)
		else
		begin
			for d1 = (-id_line) to 4 do
				if board.(d1).(d1 + id_line) != opponent_symbol then
				begin
					(* Number of same symbols in a set of 4 neighboring squares in the diagonal *)
					let count = ref 0 in
					(* Determine if there is at least one opponent symbol in the set of 4 squares *)
					let opponent = ref false in
					for d2 = d1 to (d1 + 3) do
						if board.(d2).(d2 + id_line) = symbol then
							count := !count + 1
						else if board.(d2).(d2 + id_line) = opponent_symbol then
							opponent := true
					done;
					(* Calculate the score *)
					score := !score + calculate_score_set !count symbol !opponent;
				end
			done;
		end
	end
	(* Evaluate an anti-diagonal *)
	else if line_type = "\\" then
	begin
		(* Check each possible set of 4 neighboring squares in the anti-diagonal *)
		(* First half of the board *)
		if id_line >= 0 then
		begin
			for ad1 = 7 downto (id_line + 3) do
				if board.(ad1).(id_line) != opponent_symbol then
				begin
					(* Number of same symbols in a set of 4 neighboring squares in the anti-diagonal *)
					let count = ref 0 in
					(* Determine if there is at least one opponent symbol in the set of 4 squares *)
					let opponent = ref false in
					for ad2 = ad1 downto (ad1 - 3) do
						if board.(ad2).(7 - ad2 + id_line) = symbol then
							count := !count + 1
						else if board.(ad2).(7 - ad2 + id_line) = opponent_symbol then
							opponent := true
					done;
					(* Calculating the score *)
					score := !score + calculate_score_set !count symbol !opponent;
				end
			done;
		end
		(* Second half of the board *)
		else
		begin
			for ad1 = (7 + id_line) downto 3 do
				if board.(ad1).(7 + id_line - ad1) != opponent_symbol then
				begin
					(* Number of same symbols in a set of 4 neighboring squares in the anti-diagonal *)
					let count = ref 0 in
					(* Determine if there is at least one opponent symbol in the set of 4 squares *)
					let opponent = ref false in
					for ad2 = ad1 downto (ad1 - 3) do
						if board.(ad2).(7 - ad2 + id_line) = symbol then
							count := !count + 1
						else if board.(ad2).(7 - ad2 + id_line) = opponent_symbol then
							opponent := true
					done;
					(* Calculating the score *)
					score := !score + calculate_score_set !count symbol !opponent;
				end
			done;
		end
	end;
	(* Return the score evaluated *)
	!score
;;

(* Evaluate the board to determine who has the advantage *)
let evaluate () =
	match !moves with
		| [] -> 0
		| h::t ->
			begin
				let score = ref 0 in
				(* Get the symbol of the last move *)
				let symbol = String.make 1 (L.nth !moves 0).[0] in
				(* Define the opponent symbol according to the current player symbol *)
				let opponent_symbol =
					if symbol = "X" then "O"
					else "X"
				in
				(* Evaluate rows and columns *)
				for i = 0 to 7 do
					score := !score + evaluate_line "-" i symbol opponent_symbol;
					score := !score + evaluate_line "|" i symbol opponent_symbol;
					score := !score + evaluate_line "|" i opponent_symbol symbol;
					score := !score + evaluate_line "|" i opponent_symbol symbol;
				done;
				(* Evaluate diagonals and anti-diagonals *)
				for i = -4 to 4 do
					score := !score + evaluate_line "/" i symbol opponent_symbol;
					score := !score + evaluate_line "\\" i symbol opponent_symbol;
					score := !score + evaluate_line "/" i opponent_symbol symbol;
					score := !score + evaluate_line "\\" i opponent_symbol symbol;
				done;
				(* Return the evaluated score *)
				!score;
			end
;;

(* Minimax algorithm *)
let rec minimax depth max_turn =
	let best_score = ref 0 in
	(* Best move x and y coordinates *)
	let best_move_x = ref (-1) in
	let best_move_y = ref (-1) in
	(* Initialize the best score depending on the current player *)
	if max_turn then
		(* The AI wants to maximize its score *)
		best_score := -1000000
	else
		(* The AI wants to minimize the opponent score *)
		best_score := 1000000;
	(* Symbol of the player who has to play the next move *)
	let player =
		if (String.make 1 (L.nth !moves 0).[0]) = "X" then "O"
		else "X"
	in
	(* Number of empty squares *)
	let number_of_empty_squares = L.length !empty_squares in
	(* The game is finished or the depth has been reached *)
	if (number_of_empty_squares = 0) || (depth = 0) then
		best_score := evaluate ()
	(* The game is not finished and the depth has not been reached *)
	else
	begin
		(* Try to play in each empty square *)
		L.iter (fun empty_square ->
			let move_x = (C.code (empty_square.[0]) - 48) in
			let move_y = (C.code (empty_square.[1]) - 48) in
			(* Play a move *)
			play_move player move_x move_y false;
			(* If the AI has to play the next move *)
			if max_turn then
			begin
				let current_score = L.nth (minimax (depth - 1) false) 0 in
				if current_score > !best_score then
				begin
					best_score := current_score;
					best_move_x := move_x;
					best_move_y := move_y;
				end
			end
			(* If the opponent has to play the next move *)
			else
			begin
				let current_score = L.nth (minimax (depth - 1) true) 0 in
				if current_score < !best_score then
				begin
					best_score := current_score;
					best_move_x := move_x;
					best_move_y := move_y;
				end
			end;
			(* Undo the move *)
			remove_last_move();
		) !empty_squares;
	end;
	[!best_score; !best_move_x; !best_move_y]
;;

(* Main function of the AI *)
let ai() =
	(* Get the result of the minimax function *)
	let minimax_result = minimax 2 true in
	(* Get the coordinates of the best move *)
	let move_x = L.nth minimax_result 1 in
	let move_y = L.nth minimax_result 2 in
	(* Play the move *)
	play_move "O" move_x move_y true;
	(* It is now the player turn *)
	player_turn := true
;;
