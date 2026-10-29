# Recur on a day of the month

`recur_on_day_of_month()` recurs on a specific day of the month.

## Usage

``` r
recur_on_day_of_month(x, day)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- day:

  `[integer]`

  The days of the month on which to recur. Negative values are allowed,
  which specify `n` days from the end of the month.

## Value

An updated rrule.

## Details

If the day of the month doesn't exist for that particular month, then it
is ignored. For example, if `recur_on_day_of_month(30)` is set, then it
will never generate an event in February.

## Examples

``` r
# When used with a yearly or monthly frequency, `recur_on_day_of_month()`
# expands the number of days in the event set.
on_yearly <- yearly()
on_yearly_day_of_month_1_to_2 <- on_yearly %>% recur_on_day_of_month(1:2)

start <- "1999-01-01"
end <- "2000-06-30"

alma_search(start, end, on_yearly)
#> [1] "1999-01-01" "2000-01-01"
alma_search(start, end, on_yearly_day_of_month_1_to_2)
#>  [1] "1999-01-01" "1999-01-02" "1999-02-01" "1999-02-02" "1999-03-01"
#>  [6] "1999-03-02" "1999-04-01" "1999-04-02" "1999-05-01" "1999-05-02"
#> [11] "1999-06-01" "1999-06-02" "1999-07-01" "1999-07-02" "1999-08-01"
#> [16] "1999-08-02" "1999-09-01" "1999-09-02" "1999-10-01" "1999-10-02"
#> [21] "1999-11-01" "1999-11-02" "1999-12-01" "1999-12-02" "2000-01-01"
#> [26] "2000-01-02" "2000-02-01" "2000-02-02" "2000-03-01" "2000-03-02"
#> [31] "2000-04-01" "2000-04-02" "2000-05-01" "2000-05-02" "2000-06-01"
#> [36] "2000-06-02"

# When used with a daily frequency, `recur_on_day_of_month()` limits the
# number of days in the event set.
on_daily <- daily()
on_daily_day_of_month_1_to_2 <- on_daily %>% recur_on_day_of_month(1:2)

length(alma_search(start, end, on_daily))
#> [1] 547
length(alma_search(start, end, on_daily_day_of_month_1_to_2))
#> [1] 36

# Using a negative value is a powerful way to look back from the end of the
# month. This is particularly useful because months don't have the same
# number of days.
on_last_of_month <- monthly() %>% recur_on_day_of_month(-1)

alma_search(start, end, on_last_of_month)
#>  [1] "1999-01-31" "1999-02-28" "1999-03-31" "1999-04-30" "1999-05-31"
#>  [6] "1999-06-30" "1999-07-31" "1999-08-31" "1999-09-30" "1999-10-31"
#> [11] "1999-11-30" "1999-12-31" "2000-01-31" "2000-02-29" "2000-03-31"
#> [16] "2000-04-30" "2000-05-31" "2000-06-30"

# If you want particular days of the week at the end of the month, you
# could use something like this, which checks if the end of the month
# is also a Friday.
on_last_of_month_that_is_also_friday <- on_last_of_month %>% recur_on_day_of_week("Friday")
alma_search(start, end, on_last_of_month_that_is_also_friday)
#> [1] "1999-04-30" "1999-12-31" "2000-03-31" "2000-06-30"

# But you probably wanted this, which takes the last friday of the month,
# on whatever day that lands on
on_last_friday_of_month <- monthly() %>% recur_on_day_of_week("Friday", nth = -1)
alma_search(start, end, on_last_friday_of_month)
#>  [1] "1999-01-29" "1999-02-26" "1999-03-26" "1999-04-30" "1999-05-28"
#>  [6] "1999-06-25" "1999-07-30" "1999-08-27" "1999-09-24" "1999-10-29"
#> [11] "1999-11-26" "1999-12-31" "2000-01-28" "2000-02-25" "2000-03-31"
#> [16] "2000-04-28" "2000-05-26" "2000-06-30"
```
