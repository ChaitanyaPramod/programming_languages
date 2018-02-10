(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2solution.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option ("s", ["a", "b"]) = NONE
val test1_2 = all_except_option ("s", ["a", "s", "b"]) = SOME ["a", "b"]


val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test2_2 = get_substitutions1 ([["foo", "bar"],["there"], ["quux", "foo"]], "foo") = ["bar", "quux"]


val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2 ([["foo", "bar"],["there"]], "foo") = ["bar"]
val test3_2 = get_substitutions2 ([["foo", "bar"],["there"], ["quux", "foo"]], "foo") = ["bar", "quux"]


val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color (Hearts, Num 2) = Red


val test6 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Clubs, Ace) = 11
val test6_2 = card_value (Clubs, King) = 10


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = ((remove_card ([(Hearts, King)], (Hearts, Ace), IllegalMove); false) handle IllegalMove => true)


val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false


val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Clubs, Num 2),(Clubs, Ace)] = 13


val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 2),(Clubs, King)],10) = 6
val test10_2 = score ([(Hearts, Num 2),(Diamonds, King)],10) = 3


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6


val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3


val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             

val test14_1 = score_challenge ([(Hearts, Num 2), (Clubs, Ace)], 13) = 0
val test14_2 = score_challenge ([(Hearts, Num 2), (Clubs, Ace)], 3) = 0
val test14_3 = score_challenge ([(Hearts, Ace), (Clubs, Ace)], 12) = 0
val test14_4 = score_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], 24) = 0


val test15_1 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test15_2 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        24)
             = 0
val test15_3 = ((officiate_challenge ([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)