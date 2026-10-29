# Create a custom rschedule

`rcustom()` creates an rschedule from manually defined event dates. This
can be useful when combined with [`runion()`](rset.md) and
[`rsetdiff()`](rset.md) if you have a set of fixed event dates to
forcibly include or exclude from an rschedule.

## Usage

``` r
rcustom(events)
```

## Arguments

- events:

  `[Date]`

  A vector of event dates.

## Value

A custom rschedule.

## Examples

``` r
include <- rcustom("2019-07-05")
exclude <- rcustom("2019-07-04")

independence_day <- yearly() %>%
  recur_on_month_of_year("July") %>%
  recur_on_day_of_month(4)

# Remove forcibly excluded day
independence_day <- rsetdiff(independence_day, exclude)

# Add forcibly included day
independence_day <- runion(independence_day, include)

alma_search("2018-01-01", "2020-12-31", independence_day)
#> [1] "2018-07-04" "2019-07-05" "2020-07-04"
```
