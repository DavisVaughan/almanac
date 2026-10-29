# Calendar names

`cal_names()` returns the names of the holidays in a calendar.

## Usage

``` r
cal_names(x)
```

## Arguments

- x:

  `[rcalendar]`

  An rcalendar.

## Value

A character vector of holiday names.

## Examples

``` r
x <- rcalendar(hol_christmas(), hol_new_years_day())
cal_names(x)
#> [1] "Christmas"      "New Year's Day"
```
