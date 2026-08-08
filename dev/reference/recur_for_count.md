# Control the number of times to recur

`recur_for_count()` controls the total number of events in the
recurrence set.

## Usage

``` r
recur_for_count(x, n)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- n:

  `[positive integer(1)]`

  The number of times to recur for.

## Value

An updated rrule.

## Details

Remember that the number of times the occurrence has occurred is counted
from the `since` date and is limited by the `until` date! Adjust them as
necessary to get your desired results.

## Examples

``` r
# Using the default `since` date
daily_since_epoch_for_5 <- daily() %>% recur_for_count(5)

alma_search("1969-12-31", "1970-01-25", daily_since_epoch_for_5)
#> Date of length 0

# Changing the `since` date
daily_since_2019_for_5 <- daily(since = "2019-01-01") %>% recur_for_count(5)

alma_search("2018-12-31", "2019-01-25", daily_since_2019_for_5)
#> [1] "2019-01-01" "2019-01-02" "2019-01-03" "2019-01-04" "2019-01-05"

# In the case of "impossible" dates, such as 2019-02-31 and 2019-04-31 in the
# example below, they are not added to the total count. Only true event
# dates are counted.
on_31_for_5 <- monthly(since = "2019-01-01") %>%
  recur_on_day_of_month(31) %>%
  recur_for_count(5)

alma_search("2019-01-01", "2020-01-01", on_31_for_5)
#> [1] "2019-01-31" "2019-03-31" "2019-05-31" "2019-07-31" "2019-08-31"
```
