# Recur on a month of the year

`recur_on_month_of_year()` recurs on a specific month of the year.

## Usage

``` r
recur_on_month_of_year(x, month)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- month:

  `[integer / character]`

  Months of the year to mark as events. Integer values must be between
  `[1, 12]`. This can also be a full month string like `"November"`, or
  an abbreviation like `"Nov"`.

## Value

An updated rrule.

## Examples

``` r
# There is a big difference between adding this rule to a `yearly()`
# or `monthly()` frequency, and a `daily()` frequency.

# Limit from every day to every day in February
on_feb_daily <- daily() %>% recur_on_month_of_year("Feb")

# Limit from 1 day per month to 1 day in February
on_feb_monthly <- monthly() %>% recur_on_month_of_year("Feb")

start <- "1999-01-01"
end <- "2001-01-01"

alma_search(start, end, on_feb_daily)
#>  [1] "1999-02-01" "1999-02-02" "1999-02-03" "1999-02-04" "1999-02-05"
#>  [6] "1999-02-06" "1999-02-07" "1999-02-08" "1999-02-09" "1999-02-10"
#> [11] "1999-02-11" "1999-02-12" "1999-02-13" "1999-02-14" "1999-02-15"
#> [16] "1999-02-16" "1999-02-17" "1999-02-18" "1999-02-19" "1999-02-20"
#> [21] "1999-02-21" "1999-02-22" "1999-02-23" "1999-02-24" "1999-02-25"
#> [26] "1999-02-26" "1999-02-27" "1999-02-28" "2000-02-01" "2000-02-02"
#> [31] "2000-02-03" "2000-02-04" "2000-02-05" "2000-02-06" "2000-02-07"
#> [36] "2000-02-08" "2000-02-09" "2000-02-10" "2000-02-11" "2000-02-12"
#> [41] "2000-02-13" "2000-02-14" "2000-02-15" "2000-02-16" "2000-02-17"
#> [46] "2000-02-18" "2000-02-19" "2000-02-20" "2000-02-21" "2000-02-22"
#> [51] "2000-02-23" "2000-02-24" "2000-02-25" "2000-02-26" "2000-02-27"
#> [56] "2000-02-28" "2000-02-29"

alma_search(start, end, on_feb_monthly)
#> [1] "1999-02-01" "2000-02-01"
```
