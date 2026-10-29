# Calendar matching

`cal_match()` matches a date in `x` to a holiday in `rcalendar` and
returns the corresponding holiday name, or `NA` if it doesn't exist in
the calendar.

If a date corresponds to multiple holidays, the holiday that was added
to the calendar first is returned.

This function is intended to be similar to
[`base::match()`](https://rdrr.io/r/base/match.html).

## Usage

``` r
cal_match(x, rcalendar)
```

## Arguments

- x:

  `[Date]`

  A date vector to match.

- rcalendar:

  `[rcalendar]`

  A calendar to look for holiday matches in.

## Value

A character vector the same size as `x`.

## Examples

``` r
cal <- rcalendar(
  hol_christmas(),
  hol_halloween(),
  hol_new_years_day(),
  hol_us_presidents_day()
)

x <- as.Date(c(
  "2019-01-02",
  "2019-12-25",
  "2018-02-19",
  "2018-02-20",
  "2020-10-31"
))

cal_match(x, cal)
#> [1] NA                   "Christmas"          "US Presidents' Day"
#> [4] NA                   "Halloween"         
```
