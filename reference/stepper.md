# Create a new stepper

- `stepper()` returns a function that can be used to add or subtract a
  number of days from a Date, "stepping" over events specified by an
  rschedule. You supply it the rschedule to step relative to, and then
  call the returned function with the number of days to step by.

- `workdays()` is a convenient stepper for stepping over the weekend.

- `%s+%` steps forwards.

- `%s-%` steps backwards.

You *must* use `%s+` and `%s-%` to control the stepping. `+` and `-`
will not work due to limitations in R's S3 dispatch system.
Alternatively, you can call
[`vctrs::vec_arith()`](https://vctrs.r-lib.org/reference/vec_arith.html)
directly, which powers `%s+%` with a correct double dispatch
implementation.

## Usage

``` r
stepper(rschedule)

x %s+% y

x %s-% y

workdays(n, since = NULL, until = NULL)
```

## Arguments

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

- x, y:

  `[objects]`

  Objects to perform step arithmetic on. Typically Dates or steppers.

- n:

  `[integer]`

  The number of days to step. Can be negative to step backwards.

- since:

  `[Date(1)]`

  The lower bound on the event set. Depending on the final recurrence
  rule, pieces of information from this anchor date might be used to
  generate a complete recurrence rule.

- until:

  `[Date(1)]`

  The upper bound on the event set.

## Value

- `stepper()` returns a function of 1 argument, `n`, that can be used to
  step by `n` days, relative to the rschedule.

- `workdays()` return a new stepper object.

- `%s+%` and `%s-%` return a new shifted Date vector.

## Details

Internally, a stepper is just powered by [`alma_step()`](alma_step.md),
so feel free to use that directly.

## Examples

``` r
# A Thursday and Friday
x <- as.Date(c("1970-01-01", "1970-01-02"))

# Thursday is stepped forward 1 working day to Friday,
# and then 1 more working day to Monday.
# Friday is stepped forward 1 working day to Monday,
# and then 1 more working day to Tuesday
x %s+% workdays(2)
#> [1] "1970-01-05" "1970-01-06"

# ---------------------------------------------------------------------------

on_weekends <- weekly() %>%
  recur_on_weekends()

on_christmas <- yearly() %>%
  recur_on_day_of_month(25) %>%
  recur_on_month_of_year("Dec")

rb <- runion(on_weekends, on_christmas)

workday <- stepper(rb)

# Friday before Christmas, which was on a Monday
friday_before_christmas <- as.Date("2000-12-22")

# Steps over the weekend and Christmas to the following Tuesday
friday_before_christmas %s+% workday(1)
#> [1] "2000-12-26"

# ---------------------------------------------------------------------------

# Christmas in 2005 was on a Sunday, but your company probably "observed"
# it on Monday. So when you are on the Friday before Christmas in 2005,
# stepping forward 1 working day should go to Tuesday.

# We'll adjust the previous rule for Christmas to roll to the nearest
# non-weekend day, if it happened to fall on a weekend.
on_observed_christmas <- radjusted(
  on_christmas,
  adjust_on = on_weekends,
  adjustment = adj_nearest
)

# Note that the "observed" date for Christmas is the 26th
alma_search("2005-01-01", "2006-01-01", on_observed_christmas)
#> [1] "2005-12-26"

rb2 <- runion(on_weekends, on_observed_christmas)

workday2 <- stepper(rb2)

friday_before_christmas_2005 <- as.Date("2005-12-23")

# Steps over the weekend and the observed Christmas date
# of 2005-12-26 to Tuesday the 27th.
friday_before_christmas_2005 %s+% workday2(1)
#> [1] "2005-12-27"
```
