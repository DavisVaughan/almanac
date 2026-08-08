# Step relative to an rschedule

`alma_step()` is useful for shifting dates by "n business days".

`alma_step()` steps over a sequence of dates 1 day at a time, for `n`
days. After each step, an adjustment is applied to shift to the next
non-event date.

- If `n` is positive, [`adj_following()`](adjustments.md) is called.

- If `n` is negative, [`adj_preceding()`](adjustments.md) is called.

- If `n` is zero, it was arbitrarily decided to call
  [`adj_following()`](adjustments.md) to roll to the next available
  non-event date.

## Usage

``` r
alma_step(x, n, rschedule)
```

## Arguments

- x:

  `[Date]`

  A vector of dates.

- n:

  `[integer]`

  The number of days to step. Can be negative to step backwards.

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

## Value

A Date vector the same size as `x` shifted by `n` steps.

## Details

Imagine you are on a Friday and want to shift forward 2 days using an
rrule that marks weekends as events. `alma_step()` works like this:

- Step forward 1 day to Saturday.

- Apply an adjustment of [`adj_following()`](adjustments.md), which
  rolls forward to Monday.

- Step forward 1 day to Tuesday.

- Apply an adjustment of [`adj_following()`](adjustments.md), but no
  adjustment is required.

This lends itself naturally to business logic. Two business days from
Friday is Tuesday.

## Examples

``` r
# Make a rrule for weekends
on_weekends <- weekly() %>%
  recur_on_weekends()

# "Step forward by 2 business days"
# 2019-09-13 is a Friday.
# Here we:
# - Step 1 day to Saturday
# - Adjust to Monday
# - Step 1 day to Tuesday
alma_step("2019-09-13", 2, on_weekends)
#> [1] "2019-09-17"

# If Monday, 2019-09-16, was a recurring holiday, we could create
# a custom runion and step over that too.
on_09_16 <- yearly() %>%
  recur_on_month_of_year(9) %>%
  recur_on_day_of_month(16)

rb <- runion(on_09_16, on_weekends)

alma_step("2019-09-13", 2, rb)
#> [1] "2019-09-18"
```
