(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3solution.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["zZ", "Aa","Bb","Cc", "dD"] = ["Aa","Bb","Cc"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","C", "Dd"] = "bc"


val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","C", "Dd"] = "Dd"


val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A","bc","C", "Dd"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4b_1 = longest_string4 ["A","bc","C", "Dd"] = "Dd"


val test5 = longest_capitalized ["A","bc","C"] = "A"


val test6 = rev_string "abc" = "cba"


val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = ((first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] = 4); false) handle NoAnswer => true


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test8_2 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []


val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (Variable "a") = 0


val test9b = count_wild_and_variable_lengths (Variable("a")) = 1


val test9c = count_some_var ("x", Variable("x")) = 1


val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP [Variable("x"), Variable("x")]) = false


val test11 = match (Const(1), UnitP) = NONE
val test11_1 = (not o isSome o match) (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard])

val test12 = first_match Unit [UnitP] = SOME []

(* Challenge *)
val testC_1 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT
val testC_2 = typecheck_patterns ([], [ConstP 10, Variable "a", ConstructorP("SOME",Variable “x”)]) = NONE
val testC_3 = typecheck_patterns ([], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard]) = 𝚂𝙾𝙼𝙴 𝚃𝚞𝚙𝚕𝚎𝚃[𝙰𝚗𝚢𝚝𝚑𝚒𝚗𝚐,𝙸𝚗𝚝𝚃,𝙸𝚗𝚝𝚃]
