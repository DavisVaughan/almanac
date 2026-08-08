# Generate date sequences

`alma_seq()` generates a sequence of all dates between `from` and `to`,
skipping any events defined by the `rschedule`.

## Usage

``` r
alma_seq(from, to, rschedule, inclusive = TRUE)
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

  If `from` or `to` are events in the `rschedule`, should they be
  removed from the sequence?

## Value

A vector of dates in the range of `[from, to]`, with all events in the
`rschedule` removed.

## Examples

``` r
on_weekends <- weekly() %>% recur_on_weekends()

# Generate a sequence of all non-weekend dates in Jan-2000
alma_seq("2000-01-01", "2000-01-31", on_weekends)
#>  [1] "2000-01-03" "2000-01-04" "2000-01-05" "2000-01-06" "2000-01-07"
#>  [6] "2000-01-10" "2000-01-11" "2000-01-12" "2000-01-13" "2000-01-14"
#> [11] "2000-01-17" "2000-01-18" "2000-01-19" "2000-01-20" "2000-01-21"
#> [16] "2000-01-24" "2000-01-25" "2000-01-26" "2000-01-27" "2000-01-28"
#> [21] "2000-01-31"
```
