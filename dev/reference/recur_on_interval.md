# Recur on an interval

`recur_on_interval()` adjusts the interval of the base frequency of the
recurrence rule. For example, a [`monthly()`](rrule.md) rule with an
interval of 2 would become "every other month".

## Usage

``` r
recur_on_interval(x, n)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- n:

  `[positive integer(1)]`

  The interval on which to recur.

## Value

An updated rrule.

## Examples

``` r
# The default interval is 1
on_monthly <- monthly(since = "1999-01-01")

alma_search("1999-01-01", "1999-06-01", on_monthly)
#> [1] "1999-01-01" "1999-02-01" "1999-03-01" "1999-04-01" "1999-05-01"
#> [6] "1999-06-01"

# Adjust to every other month
on_every_other_month <- on_monthly %>% recur_on_interval(2)

alma_search("1999-01-01", "1999-06-01", on_every_other_month)
#> [1] "1999-01-01" "1999-03-01" "1999-05-01"

# Note that the frequency is limited to "every other month", but you
# can still have multiple events inside a single month
on_every_other_month_on_day_25_or_26 <- on_every_other_month %>%
  recur_on_day_of_month(25:26)

alma_search("1999-01-01", "1999-06-01", on_every_other_month_on_day_25_or_26)
#> [1] "1999-01-25" "1999-01-26" "1999-03-25" "1999-03-26" "1999-05-25"
#> [6] "1999-05-26"
```
