# Deprecated rset helpers

**\[deprecated\]**

- `add_rschedule()` is deprecated in favor of using the `...` arguments
  of [`runion()`](rset.md), [`rintersect()`](rset.md), and
  [`rsetdiff()`](rset.md) directly.

- `add_rdates()` is deprecated in favor of using [`runion()`](rset.md)
  in combination with a custom [`rcustom()`](rcustom.md) rschedule
  holding the required dates.

- `add_exdates()` is deprecated in favor of using
  [`rsetdiff()`](rset.md) in combination with a custom
  [`rcustom()`](rcustom.md) rschedule holding the excluded dates.

## Usage

``` r
add_rschedule(x, rschedule)

add_rdates(x, rdates)

add_exdates(x, exdates)
```

## Arguments

- x:

  `[rset]`

  An rset to add to.

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

- rdates:

  `[Date]`

  Dates to forcibly include in the rset.

- exdates:

  `[Date]`

  Dates to forcibly exclude from the rset.

## Value

An updated rset.

## Examples

``` r
on_thanksgiving <- yearly() %>%
  recur_on_day_of_week("Thurs", nth = 4) %>%
  recur_on_month_of_year("Nov")

on_christmas <- yearly() %>%
  recur_on_day_of_month(25) %>%
  recur_on_month_of_year("Dec")

on_labor_day <- monthly() %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_week("Mon", nth = 1)

# Rather than:
if (FALSE) {
rb <- runion() %>%
  add_rschedule(on_thanksgiving) %>%
  add_rschedule(on_christmas) %>%
  add_rschedule(on_labor_day)
}

# Use the `...` of the `runion()` helper directly:
rb <- runion(on_thanksgiving, on_christmas, on_labor_day)

# Thanksgiving, Christmas, or Labor Day
alma_search("2019-01-01", "2021-01-01", rb)
#> [1] "2019-09-02" "2019-11-28" "2019-12-25" "2020-09-07" "2020-11-26"
#> [6] "2020-12-25"

# Except Labor Day in 2019
# Rather than:
if (FALSE) {
rb2 <- add_exdates(rb, "2019-09-02")
}

# We recommend:
rb2 <- rsetdiff(rb, rcustom("2019-09-02"))

alma_search("2019-01-01", "2021-01-01", rb2)
#> [1] "2019-11-28" "2019-12-25" "2020-09-07" "2020-11-26" "2020-12-25"
```
