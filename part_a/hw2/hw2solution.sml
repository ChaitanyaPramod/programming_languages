(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1.(a) *)
fun all_except_option (_, []) = NONE
  | all_except_option (s, x::xs) =
  if same_string(s, x)
  then SOME xs
  else case all_except_option(s, xs) of
	      NONE => NONE
	    | SOME xs' => SOME(x::xs')


(* 1.(b) *)
(* fun get_substitutions1 ([substitutions : string list list], s : string) = *)
fun get_substitutions1 ([], _) = []
  | get_substitutions1 (x::xs, s) =
    case all_except_option(s, x) of
        NONE => get_substitutions1(xs, s)
      | SOME xs' => xs' @ get_substitutions1 (xs, s)


(* 1.(c) *)
fun get_substitutions2 (xss, s) =
    let
    	fun add_substitutions ([], _, acc) = acc
		  | add_substitutions (xs::xss', s, acc) =
		  case all_except_option(s, xs) of
		      NONE => add_substitutions(xss', s, acc)
		    | SOME xs' => add_substitutions (xss', s, acc @ xs')
    in
    	add_substitutions (xss, s, [])
    end

(* 1.(d) *)

fun similar_names (xss, {first=first_name, middle=middle_name, last=last_name}) =
	let
		val all_first_names = first_name::get_substitutions2(xss, first_name)

		fun similar ([]) = []
		  | similar (xs::xss') =
		    {first=xs, middle=middle_name, last=last_name}::similar(xss')
	in
		similar(all_first_names)
	end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2.(a) *)
fun card_color (Clubs, _) = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _) = Red
  | card_color (Spades, _) = Black


(* 2.(b) *)
fun card_value (_, Jack) = 10
  | card_value (_, Queen) = 10
  | card_value (_, King) = 10
  | card_value (_, Ace) = 11
  | card_value (_, Num n) = n


(* 2.(c) *)
fun remove_card ([], _, e) = raise e
  | remove_card (first::cs', c, e) =
	  if first = c
	  then cs'
	  else first::remove_card(cs', c, e);


(* 2.(d) *)
fun all_same_color ([]) = true
  | all_same_color (head::[]) = true
  | all_same_color (head::neck::tail) =
  	card_color(head) = card_color(neck) andalso
  	all_same_color(neck::tail)


(* 2.(e) *)
fun sum_cards cs =
	let
		fun sum([], acc) = acc
		  | sum(c::cs', acc) = sum(cs', card_value(c) + acc);
	in
		sum(cs, 0)
	end


(* 2.(f) *)
fun score (held_cards, goal) =
	let
		val sum = sum_cards held_cards
		val is_same_color = all_same_color held_cards
		val is_sum_greater = sum > goal
		val prelim_score =
			if is_sum_greater
			then 3 * (sum - goal)
			else (goal - sum)
	in
		if is_same_color
		then prelim_score div 2
		else prelim_score 
	end


(* 2.(g) *)
fun officiate (cards, moves, goal) =
	let
		fun make_move (args) = case args of
			(held_cards, cardlist, []) => score(held_cards, goal)
		  | (held_cards, cardlist, (Discard c)::moves') => make_move (remove_card(held_cards, c, IllegalMove), cardlist, moves')
	      | (held_cards, [], Draw::_) => score(held_cards, goal)
	      | (held_cards, c::cs, Draw::moves') =>
      			if sum_cards(c::held_cards) > goal
      			then score(c::held_cards, goal)
  				else make_move(c::held_cards, cs, moves')
	in
		make_move([], cards, moves)
	end


(* 3.(a) *)

(* Helper *)
fun min (x::[]) = x
  | min (x::xs) = Int.min(x, min(xs))

fun map ([], f) = []
  | map (x::xs, f) = f(x)::map(xs, f)

fun all_possible_sums (cs) =
	let
        fun all_possible_aces_sums (aces) =
        	let
        		fun helper (n, 0) = [n]
        		  | helper (n, i) = ((n - i) + i * 11)::helper(n, i - 1)
        	in
        		helper (aces, aces)
        	end

		fun num_aces ([]) = 0
		  | num_aces ((_, Ace)::cs) = 1 + num_aces cs
		  | num_aces (_::cs) = num_aces cs

	    fun sum_without_aces (cs) = case cs of
        	[] => 0
          | (_, Ace)::cs => sum_without_aces cs
          | c::cs' => card_value(c) + sum_without_aces(cs')

		val num_of_aces = num_aces cs
		val without_aces_sum = sum_without_aces cs
		val aces_sums = all_possible_aces_sums num_of_aces
		fun add (s) = s + without_aces_sum
	in
		map (aces_sums, add)
	end

fun score_challenge (held_cards, goal) =
	let
		val is_same_color = all_same_color held_cards

		fun prelim_score (sum) =
			if sum > goal
			then 3 * (sum - goal)
			else (goal - sum)

		fun final_score (prelim) =
			if is_same_color
			then prelim div 2
			else prelim

		fun scores(sums) =
			map (map (sums, prelim_score), final_score)
	in
		min (scores (all_possible_sums (held_cards) ) )
	end


(* 3.(a) *)
fun officiate_challenge (cards, moves, goal) =
	let
		fun make_move (args) = case args of
			(held_cards, cardlist, []) => score_challenge(held_cards, goal)
		  | (held_cards, cardlist, (Discard c)::moves') => make_move (remove_card(held_cards, c, IllegalMove), cardlist, moves')
	      | (held_cards, [], Draw::_) => score_challenge(held_cards, goal)
	      | (held_cards, c::cs, Draw::moves') =>
      			if min (all_possible_sums(c::held_cards)) > goal
      			then score_challenge(c::held_cards, goal)
  				else make_move(c::held_cards, cs, moves')
	in
		make_move([], cards, moves)
	end


(* 3.(b) *)
fun careful_player(cards, goal) =
	let
		fun make_next_move (held_cards, cs, prev_moves) =
			if (score(held_cards, goal) = 0)
			then prev_moves
			else
				if (sum_cards(held_cards) + 10) < goal
				then case cs of
					[] => prev_moves @ [Draw]
				  | c::cs' => make_next_move (c::held_cards, cs', prev_moves @ [Draw])
				else case (held_cards, cs) of
					(h::hs, )
	in
		make_next_move ([], cards, [])
	end
	

