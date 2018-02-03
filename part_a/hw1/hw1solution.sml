fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    let
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1

        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2
        orelse (y1 = y2 andalso m1 < m2)
        orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end

fun number_in_month (ds : ((int * int * int) list), m : int) =
    if null ds
    then 0
    else
        let
            val tl_number = number_in_month(tl ds, m)
        in
            if #2 (hd ds) = m
            then 1 + tl_number
            else tl_number
        end


fun number_in_months (ds : ((int * int * int) list), ms : int list) =
    if null ms
    then 0
    else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)


fun dates_in_month (ds : ((int * int * int) list), m : int) =
    if null ds
    then []
    else
        let
            val tl_dates = dates_in_month(tl ds, m)
        in
            if #2 (hd ds) = m
            then hd ds::tl_dates
            else tl_dates
        end


fun dates_in_months (ds : ((int * int * int) list), ms : int list) =
    if null ms
    then []
    else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)


fun get_nth (ls : string list, n : int) =
    if n = 1
    then hd ls
    else get_nth(tl ls, n - 1)


fun date_to_string (date : int * int * int) =
    let
        val months = ["January", "February", "March", "April", "May",
        "June", "July", "August", "September", "October", "November",
        "December"]
    in
        get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


fun number_before_reaching_sum (sum : int, ns : int list) =
    if hd ns >= sum
    then 0
    else
        1 + number_before_reaching_sum(sum - hd ns, tl ns)


fun what_month (day_of_year : int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day_of_year, days_in_month) + 1
    end


fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
        what_month day1 :: month_range(day1 + 1, day2)


fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            fun oldest_non_empty (ds : (int * int * int) list) =
                if null(tl ds)
                then hd ds
                else
                    let
                        val tl_oldest = oldest_non_empty(tl ds)
                    in
                        if is_older(hd ds, tl_oldest)
                        then hd ds
                        else tl_oldest
                    end
        in
            SOME(oldest_non_empty dates)
        end

fun deduplicate (ns : int list) =
    let
        fun list_contains (l : int list, n : int) =
            if null l
            then false
            else
                n = hd l orelse list_contains(tl l, n)

        fun add_to_list (deduped : int list, new : int list) =
            if null new
            then deduped
            else
                if list_contains(deduped, hd new)
                then add_to_list(deduped, tl new)
                else add_to_list(hd new :: deduped, tl new)
    in
        add_to_list([], ns)
    end

fun number_in_months_challenge (ds : ((int * int * int) list), ms : int list) =
    number_in_months(ds, deduplicate ms)


fun dates_in_months_challenge (ds : ((int * int * int) list), ms : int list) =
    dates_in_months(ds, deduplicate ms)


fun reasonable_date (d : int * int * int) =
    let
        val days_in_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        fun is_leap_year (yr : int) =
            if yr mod 4 <> 0
            then false
            else yr mod 100 <> 0 orelse yr mod 400 = 0

        fun get_nth (ls : int list, n : int) =
            if n = 1
            then hd ls
            else get_nth(tl ls, n - 1)

        val year = #1 d
        val month = #2 d
        val day = #3 d

        val is_leap_feb = is_leap_year year andalso month = 2
    in
        year > 0 andalso
        month >= 1 andalso
        month <= 12 andalso
        day > 0 andalso        
        day <= get_nth(days_in_month, month) +
            (if is_leap_feb then 1 else 0)
    end
