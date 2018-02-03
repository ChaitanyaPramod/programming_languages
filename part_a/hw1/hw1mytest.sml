

use "hw1solution.sml";
use "hw1test.sml";

val test1_1 = is_older ((1,2,3),(2,3,4)) = true
val test1_2 = is_older ((2,3,4),(1,2,3)) = false
val test1_3 = is_older ((1,2,3),(1,2,3)) = false
val test1_4 = is_older ((1,2,3),(1,2,4)) = true


val test2_1 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_2 = number_in_month ([(2012,2,28),(2013,12,1)],10) = 0


val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3


val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]


val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]


val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"


val test7_1 = date_to_string (2013, 6, 1) = "June 1, 2013"


val test8_1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3


val test9_1 = what_month 70 = 3
val test9_2 = what_month 31 = 1
val test9_3 = what_month 32 = 2


val test10_1 = month_range (31, 34) = [1,2,2,2]


val test11_1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)


val test_dedup_1 = deduplicate([1,2,3]) = [3,2,1]
val test_dedup_2 = deduplicate([1,2,1]) = [2,1]
val test_dedup_3 = deduplicate([1,1]) = [1]


val test12_1 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,2,3]) = 2
val test12_2 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,4]) = [(2011,4,28),(2011,3,31),(2012,2,28)]

val test13_1 = reasonable_date (2016, 2, 29) = true
val test13_2 = reasonable_date (2016, 3, 31) = true
val test13_3 = reasonable_date (2016, 4, 31) = false
val test13_4 = reasonable_date (2017, 2, 29) = false
val test13_5 = reasonable_date (2000, 2, 29) = true
val test13_6 = reasonable_date (2100, 2, 29) = false