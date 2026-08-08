# US federal calendar

`cal_us_federal()` is an example calendar that represents the federal
holidays in the United States. It makes no attempt to be historically
accurate, but instead represents the *currently* recognized federal
holidays. The calendar represents the *observed* dates of each holiday,
rather than the actual dates of each holiday (i.e. if a holiday falls on
a Saturday, it is federally observed on the preceding Friday).

Refer to the source code of `cal_us_federal()` to get a feel for how to
build your own personal calendar.

## Usage

``` r
cal_us_federal(since = NULL, until = NULL)
```

## Arguments

- since:

  `[Date(1)]`

  A lower bound on the event set to generate.

  Defaults to [`almanac_since()`](almanac-defaults.md) if not set.

- until:

  `[Date(1)]`

  An upper bound on the event set to generate.

  Defaults to [`almanac_until()`](almanac-defaults.md) if not set.

## Value

An rcalendar.

## Examples

``` r
cal <- cal_us_federal()

# All 2023 holidays
cal_events(cal, year = 2023)
#>                             name       date
#> 1                 New Year's Day 2023-01-02
#> 2  US Martin Luther King Jr. Day 2023-01-16
#> 3             US Presidents' Day 2023-02-20
#> 4                US Memorial Day 2023-05-29
#> 5                  US Juneteenth 2023-06-19
#> 6            US Independence Day 2023-07-04
#> 7                   US Labor Day 2023-09-04
#> 8     US Indigenous Peoples' Day 2023-10-09
#> 9                US Veterans Day 2023-11-10
#> 10               US Thanksgiving 2023-11-23
#> 11                     Christmas 2023-12-25

# Notice that for 2028, `cal_events()` knows that you probably want to
# treat New Year's Day as a 2028 holiday even though it will observed in
# 2027 (because it will be a Saturday and will be rolled back to being
# observed on Friday)
cal_events(cal, year = 2028)
#>                             name       date
#> 1                 New Year's Day 2027-12-31
#> 2  US Martin Luther King Jr. Day 2028-01-17
#> 3             US Presidents' Day 2028-02-21
#> 4                US Memorial Day 2028-05-29
#> 5                  US Juneteenth 2028-06-19
#> 6            US Independence Day 2028-07-04
#> 7                   US Labor Day 2028-09-04
#> 8     US Indigenous Peoples' Day 2028-10-09
#> 9                US Veterans Day 2028-11-10
#> 10               US Thanksgiving 2028-11-23
#> 11                     Christmas 2028-12-25

# Were any of these dates on a holiday?
x <- as.Date(c(
  "2023-11-10",
  "2023-10-05",
  "2023-06-19",
  "2023-05-29",
  "2023-05-28"
))

alma_in(x, cal)
#> [1]  TRUE FALSE  TRUE  TRUE FALSE

# Which one?
cal_match(x, cal)
#> [1] "US Veterans Day" NA                "US Juneteenth"   "US Memorial Day"
#> [5] NA               
```
