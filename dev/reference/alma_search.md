# Search for events

`alma_search()` retrieves all events between `from` and `to`.

## Usage

``` r
alma_search(from, to, rschedule, inclusive = TRUE)
```

## Arguments

- from, to:

  `[Date(1)]`

  Dates defining the range to look for events.

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

- inclusive:

  `[logical(1)]`

  If `from` or `to` are events, should they be included?

## Value

A Date vector of all events between `from` and `to`.

## Examples

``` r
on_12th <- monthly() %>% recur_on_day_of_month(12)
on_monday <- weekly() %>% recur_on_day_of_week("Monday")

# On the 12th of the month, or on Mondays
rb <- runion(on_12th, on_monday)

alma_search("2019-01-01", "2019-01-31", rb)
#> [1] "2019-01-07" "2019-01-12" "2019-01-14" "2019-01-21" "2019-01-28"
```
