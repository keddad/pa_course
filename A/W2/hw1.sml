fun is_older(a: int*int*int, b: int*int*int) =
    (#1 a < #1 b) orelse (#1 a = #1 b andalso #2 a < #2 b) orelse (#1 a = #1 b andalso #2 a = #2 b andalso #3 a < #3 b)

fun number_in_month(ds: (int*int*int) list, m: int) = 
    let
      fun rec_cnt(xs: (int*int*int) list, cnt: int) = 
        if null xs
        then cnt
        else if #2 (hd xs) = m
        then rec_cnt(tl xs, cnt + 1)
        else rec_cnt(tl xs, cnt)
    in
      rec_cnt(ds, 0)
    end

fun number_in_months(ds: (int*int*int) list, ms: int list) = 
    let
      fun rec_cnt(rds: (int*int*int) list, rms: int list, cnt: int) = 
        if null rms
        then cnt
        else rec_cnt(rds, tl rms, cnt + number_in_month(rds, hd rms))
    in
      rec_cnt(ds, ms, 0)
    end

fun dates_in_month(ds: (int*int*int) list, m: int) = 
    if null ds
    then []
    else if #2 (hd ds) = m
    then (hd ds) :: dates_in_month(tl ds, m)
    else dates_in_month(tl ds, m)

fun dates_in_months(ds: (int*int*int) list, ms: int list) =
    let
      fun rec_cnt(rms: int list) =
        if null rms
        then []
        else dates_in_month(ds, hd rms) @ rec_cnt(tl rms)
    in
      rec_cnt ms
    end

fun get_nth(sts: string list, n: int) =
    if n = 1
    then hd sts
    else get_nth(tl sts, n-1)

fun date_to_string(date: (int*int*int)) =
    let
      val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
      get_nth(months, (#2 date)) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

fun number_before_reaching_sum(sum: int, xs: int list) =
    let
      fun rec_cnt(rsum: int, cnt: int, rxs: int list) =
        if rsum <= 0
        then cnt
        else rec_cnt(rsum - hd rxs, cnt + 1, tl rxs)
    in
      rec_cnt(sum, ~1, xs)
    end

fun what_month(d: int) =
    let
      val splits = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      number_before_reaching_sum(d, splits) + 1
    end

fun month_range(a: int, b: int) =
    if a > b
    then []
    else let
      fun rec_calc(cur: int) =
        if cur > b
        then []
        else what_month(cur) :: rec_calc(cur+1)
    in
      rec_calc(a)
    end

fun oldest(ds: (int*int*int) list) =
    if null ds
    then NONE
    else
        let
            val max_tail = oldest(tl ds)
            fun mx_date(a: (int*int*int), b: (int*int*int)) =
                if is_older(b, a)
                then b
                else a
        in
            if isSome max_tail
            then SOME (mx_date(hd ds, valOf max_tail))
            else SOME (hd ds)
        end