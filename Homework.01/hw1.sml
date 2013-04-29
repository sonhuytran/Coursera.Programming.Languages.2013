fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if
	#3 (date1) < #3(date2)
	orelse (#3 (date1) = #3(date2)
		andalso (#2 (date1) < #2 (date2)
			  orelse (#2 (date1) = #2 (date2)
				   andalso #1 (date1) < #1 (date2))))
    then true
    else false

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null (dates)
    then 0
    else if #2 (hd (dates)) = month
    then 1 + number_in_month (tl (dates), month)
    else number_in_month (tl (dates), month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null (months)
    then 0
    else number_in_month (dates, hd (months))
	 + number_in_months (dates, tl (months))

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null (dates)
    then []
    else if #2 (hd (dates)) = month
    then hd (dates) :: dates_in_month (tl (dates), month)
    else dates_in_month (tl (dates), month)

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null (months)
    then []
    else dates_in_month (dates, hd (months))
	 @ dates_in_months (dates, tl (months))

fun get_nth (strings : string list, n : int) =
    if null (strings)
       orelse n < 1
    then ""
    else if n = 1
    then hd (strings)
    else get_nth (tl (strings), n - 1)

fun date_to_string (date : int * int * int) =
    let
	val month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth (month_names, #2 (date))
	^ " "
	^ Int.toString (#1 (date))
	^ ", "
	^ Int.toString (#3 (date))
    end

fun number_before_reaching_sum (sum : int, values : int list) =
    if null (values)
    then 0
    else if hd (values) >= sum
    then 0
    else 1 + number_before_reaching_sum (sum - hd (values), tl (values))

fun what_month (day_of_year : int) =
    let
	val dates_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	if day_of_year < 1
	   orelse day_of_year > 365
	then 0
	else number_before_reaching_sum (day_of_year, dates_in_months) + 1
    end

fun month_range (day_of_year1 : int, day_of_year2 : int) = 
    if day_of_year1 > day_of_year2
    then []
    else what_month (day_of_year1)
	 :: month_range (day_of_year1 + 1, day_of_year2)

fun oldest (dates : (int * int * int) list) =
    if null (dates)
    then NONE
    else if null (tl (dates))
    then SOME (hd (dates))
    else
	let
	    val oldest_tail = oldest (tl (dates))
	in
	    if is_older (hd (dates), valOf (oldest_tail))
	    then SOME (hd (dates))
	    else oldest_tail
	end

fun is_member (values : int list, value : int) =
    if null (values)
    then false
    else if (value = hd (values))
    then true
    else is_member (tl (values), value)

fun remove_duplicates (values : int list) =
    if null (values)
    then []
    else if null (tl (values))
    then values
    else if is_member (tl (values), hd (values))
    then remove_duplicates (tl (values))
    else hd (values)
	 :: remove_duplicates (tl (values))

fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months (dates, remove_duplicates (months))

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months (dates, remove_duplicates (months))

fun reasonable_date (dates : int * int * int) =
    let
	val date = #1 (dates)
	val month = #2 (dates)
	val year = #3 (dates)
	val months31 = [1, 3, 5, 7, 8, 10, 12]
	val months30 = [4, 6, 9, 11]
	
	fun leap_year (year : int) =
	    if year mod 400 = 0
	       orelse (year mod 4 = 0
		       andalso year mod 100 <> 0)
	    then true
	    else false
    in
	if year > 0
	   andalso month >= 1
	   andalso month <= 12
	   andalso date > 0
	   andalso ((is_member (months31, month)
		     andalso date <=31)
		    orelse (is_member (months30, month)
			    andalso date <= 30)
		    orelse (leap_year(year)
			    andalso date <= 29)
		    orelse date <= 28)
	then true
	else false
    end
