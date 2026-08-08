# Create a new set-based recurrence schedule

Often, a single rrule will be sufficient. However, more complex
recurrence objects can be constructed by combining multiple rschedules
into a *recurrence set*.

There are three types of recurrence sets provided in almanac, each of
which construct their event sets by performing a set operation on the
underlying events of the rschedules in the set.

- `runion()` takes the union.

- `rintersect()` takes the intersection.

- `rsetdiff()` takes the set difference.

## Usage

``` r
runion(...)

rintersect(...)

rsetdiff(...)
```

## Arguments

- ...:

  `[rschedules]`

  rschedule objects to add to the set.

## Value

A runion, rintersect, or rsetdiff.

## Details

For `rsetdiff()`, the event set is created "from left to right" and
depends on the order that the rschedules were added to the set.

## Examples

``` r
since <- "2019-04-01"
until <- "2019-05-31"

on_weekends <- weekly(since = since, until = until) %>%
  recur_on_weekends()

on_25th <- monthly(since = since, until = until) %>%
  recur_on_day_of_month(25)

# On weekends OR the 25th of the month
ru <- runion(on_weekends, on_25th)
alma_events(ru)
#>  [1] "2019-04-06" "2019-04-07" "2019-04-13" "2019-04-14" "2019-04-20"
#>  [6] "2019-04-21" "2019-04-25" "2019-04-27" "2019-04-28" "2019-05-04"
#> [11] "2019-05-05" "2019-05-11" "2019-05-12" "2019-05-18" "2019-05-19"
#> [16] "2019-05-25" "2019-05-26"

# On weekends AND the 25th of the month
ri <- rintersect(on_weekends, on_25th)
alma_events(ri)
#> [1] "2019-05-25"

# On weekends AND NOT the 25th of the month
rsd1 <- rsetdiff(on_weekends, on_25th)
alma_events(rsd1)
#>  [1] "2019-04-06" "2019-04-07" "2019-04-13" "2019-04-14" "2019-04-20"
#>  [6] "2019-04-21" "2019-04-27" "2019-04-28" "2019-05-04" "2019-05-05"
#> [11] "2019-05-11" "2019-05-12" "2019-05-18" "2019-05-19" "2019-05-26"

# On the 25th of the month AND NOT the weekend
rsd2 <- rsetdiff(on_25th, on_weekends)
alma_events(rsd2)
#> [1] "2019-04-25"
```
