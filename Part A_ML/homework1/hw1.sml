(* Assignment 1 *)

fun is_older (d1: int*int*int, d2: int*int*int) =
    if (#1 d1) < (#1 d2)
    then true
    else if (#1 d1) > (#1 d2) 
    then false (*comparing from year to day*)
    else if (#2 d1) < (#2 d2)
    then true
    else if (#2 d1) > (#2 d2)
    then false
    else if (#3 d1) < (#3 d2)
    then true
    else false
	     
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates orelse month > 12 (*check empty list or wrong month*)
    then 0
    else if (#2 (hd dates)) = month (*check each month from the list, then recurses it*)
    then 1 + number_in_month((tl dates),month)
    else number_in_month((tl dates),month)

fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null dates orelse null months (*check empty list or months*)
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))
         (*use number_in_month to recurse each month with the list of dates*)

fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates orelse month > 12 (*check empty list or wrong month*)
    then [] 
    else if (#2 (hd dates)) = month (*check each month from the list, then recurses it*)
    then (hd dates) :: dates_in_month((tl dates), month) 
    else dates_in_month((tl dates), month)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null dates orelse null months (*check empty list or months*)
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))
	(*use dates_in_month to recurse each month with the list of dates and add the list with @*)

fun get_nth (strings: string list, nth: int) =
    if null strings (*check empty list*)
    then ""
    else if nth = 1 (*check whether nth is accumulated to 0*)
    then (hd strings)
    else get_nth((tl strings), nth-1)

fun date_to_string (date: (int*int*int)) =
    let val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] (*list of the twelve months' strings*)
	val month = get_nth(month_names, (#2 date))
	val day = (Int.toString (#3 date))
        val year = (Int.toString (#1 date))
     in month^" "^day^", "^year
     end				      

fun number_before_reaching_sum (sum: int, numbers: int list) =
     if sum - (hd numbers) > 0  (*if sum - n greater than 0*)
     then 1 + number_before_reaching_sum(sum-(hd numbers), (tl numbers))
     else 0

fun what_month (day: int) =
    let val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in number_before_reaching_sum(day, months) + 1 (*because what_month takes the day after reaching the month, so add 1 after number_before_reaching_sum*)
    end

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1,day2) (*find the month of each mn with what_month*)

fun oldest (dates: (int*int*int) list) =
    if null dates
    then NONE
    else if null (tl dates)
    then SOME (hd dates)
    else
	let val tl_ans = oldest(tl dates)
	in if is_older((hd dates),(valOf tl_ans))
	then SOME (hd dates)
	else tl_ans
        end

(*fun remove_duplicates is the helper function for number_in_months_challenge and dates_in months_challegnge, to remove deplicates month in the list*)
fun remove_duplicates (months: int list) =
    if null months
    then []
    else let fun check_repeated (value: int, checklist: int list) = (*function for checking repeated months in list*)
	     if null checklist
             then false
	     else if value = (hd checklist)
	     then true
	     else check_repeated (value, (tl checklist))
	 in if check_repeated((hd months), (tl months))
	    then remove_duplicates((tl months))
	    else (hd months) :: remove_duplicates((tl months))
	 end
		     
fun number_in_months_challenge (dates: (int*int*int) list, months: int list) =
    number_in_months(dates, remove_duplicates(months)) (*after removed duplicated months*)

fun dates_in_months_challenge (dates: (int*int*int) list, months: int list) =
    dates_in_months(dates, remove_duplicates(months)) (*after removed duplicated months*)


fun reasonable_date (date: (int*int*int)) =
    if (#1 date) < 1                                                          (*basic check for year greater than 1*)
    then false
    else if (#2 date) < 1 orelse (#2 date) > 12                               (*basic check for month within 1 to 12*)
    then false
    else let val days_leap = [31,29,31,30,31,30,31,31,30,31,30,31]            (*leap year month's days list*)
	     val days_normal = [31,28,31,30,31,30,31,31,30,31,30,31]          (*normal year month's days list*)
	     fun is_leap (year : int) =                                       (*function for checking the given year is leap year or not*)
	      ((year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0))
	     val days = if is_leap(#1 date) then days_leap else days_normal 
	 in  if (#3 date) < 1 orelse (#3 date) > List.nth(days, (#2 date) - 1)(*use the built-in function List.nth to find out the month's days from the list of days*)
	     then false
	     else true
	 end
