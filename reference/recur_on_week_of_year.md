# Recur on a week of the year

`recur_on_week_of_year()` recurs on a specific week of the year.

## Usage

``` r
recur_on_week_of_year(x, week)
```

## Arguments

- x:

  `[rrule]`

  A recurrence rule.

- week:

  `[integer]`

  Weeks of the year to recur on. Integer values must be between
  `[1, 53]` or `[-53, -1]`.

## Value

An updated rrule.

## Details

Weekly rules are implemented according to the ISO-8601 standard. This
requires that the first week of a year is the first one containing at
least 4 days of the new year. Additionally, the week will start on the
week day specified by
[`recur_with_week_start()`](recur_with_week_start.md), which defaults to
Monday.

## Examples

``` r
# Weekly rules are a bit tricky because they are implemented to comply
# with ISO-8601 standards, which require that the first week of the year
# is when there are at least 4 days in that year, and the week starts on
# the week day specified by `recur_with_week_start()` (Monday by default).
on_first_week <- yearly() %>% recur_on_week_of_year(1)

# In 2017:
# - Look at dates 1-4
# - 2017-01-02 is a Monday, so start the first week here
alma_search("2017-01-01", "2017-01-25", on_first_week)
#> [1] "2017-01-02" "2017-01-03" "2017-01-04" "2017-01-05" "2017-01-06"
#> [6] "2017-01-07" "2017-01-08"

# In 2015:
# - Look at dates 1-4
# - None of these are Monday, so the start of the week is
#   in the previous year
# - Look at 2014 and find the last Monday, 2014-12-29. This is the start of
#   the first week in 2015.
alma_search("2014-12-25", "2015-01-25", on_first_week)
#> [1] "2014-12-29" "2014-12-30" "2014-12-31" "2015-01-01" "2015-01-02"
#> [6] "2015-01-03" "2015-01-04"

# Say we want the start of the week to be Sunday instead of Monday!

# In 2015:
# - Look at dates 1-4
# - 2015-01-04 is a Sunday, so start the first week here
on_first_week_sun <- yearly() %>%
  recur_on_week_of_year(1) %>%
  recur_with_week_start("Sunday")

alma_search("2014-12-25", "2015-01-25", on_first_week_sun)
#> [1] "2015-01-04" "2015-01-05" "2015-01-06" "2015-01-07" "2015-01-08"
#> [6] "2015-01-09" "2015-01-10"
```
