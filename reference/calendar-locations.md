# Calendar locations

- `cal_next()` generates the next holiday after `x`.

- `cal_previous()` generates the previous holiday before `x`.

If no holiday exists before/after `x`, a missing row is generated.

## Usage

``` r
cal_next(x, rcalendar, ..., inclusive = FALSE)

cal_previous(x, rcalendar, ..., inclusive = FALSE)
```

## Arguments

- x:

  `[Date]`

  A vector of dates.

- rcalendar:

  `[rcalendar]`

  An rcalendar.

- ...:

  These dots are for future extensions and must be empty.

- inclusive:

  `[logical(1)]`

  If `x` is an event, should it be considered the next or previous
  event?

## Value

A two column data frame, like [`cal_events()`](cal_events.md), which is
the same size as `x` and contains either the next or previous holiday
relative to `x`.

## Examples

``` r
x <- as.Date(c("2023-04-11", "2023-08-10", "2021-05-06"))
cal <- cal_us_federal()

cal_next(x, cal)
#>              name       date
#> 1 US Memorial Day 2023-05-29
#> 2    US Labor Day 2023-09-04
#> 3 US Memorial Day 2021-05-31
cal_previous(x, cal)
#>                  name       date
#> 1  US Presidents' Day 2023-02-20
#> 2 US Independence Day 2023-07-04
#> 3  US Presidents' Day 2021-02-15
```
