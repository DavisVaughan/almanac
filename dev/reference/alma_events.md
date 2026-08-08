# Get all events

`alma_events()` retrieves all of the events in the rschedule's event
set.

## Usage

``` r
alma_events(rschedule, ..., year = NULL)
```

## Arguments

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

- ...:

  These dots are for future extensions and must be empty.

- year:

  `[NULL / integer]`

  An optional integer vector of years to limit the returned events to.

## Value

A Date vector of events.

## Examples

``` r
rrule <- daily(since = "1970-01-01", until = "1970-01-05")

alma_events(rrule)
#> [1] "1970-01-01" "1970-01-02" "1970-01-03" "1970-01-04" "1970-01-05"

on_christmas <- yearly() %>%
  recur_on_month_of_year("Dec") %>%
  recur_on_day_of_month(25)

alma_events(on_christmas, year = c(2020, 2022))
#> [1] "2020-12-25" "2022-12-25"
```
