# Calendar events

`cal_events()` returns a data frame of holiday name / event date pairs
for a calendar. It is similar to [`alma_events()`](alma_events.md), but
returns information about the name of the holiday and has specialized
behavior related to observed dates when filtering by `year`.

## Usage

``` r
cal_events(x, ..., year = NULL, observed = FALSE)
```

## Arguments

- x:

  `[rcalendar]`

  An rcalendar.

- ...:

  These dots are for future extensions and must be empty.

- year:

  `[integer]`

  An integer vector of years to filter for.

- observed:

  `[FALSE / TRUE]`

  When filtering for specific `year`s, should the *observed* date of the
  holiday be used for filtering purposes? If `FALSE`, the *actual* date
  of the holiday will be used, i.e. the date before any observance
  adjustments created by [`hol_observe()`](holiday-utilities.md) have
  been applied, which is typically desired when filtering for a year's
  worth of holidays. See the examples.

## Value

A two column data frame:

- `name` is a character vector of holiday names.

- `date` is a Date vector of holiday event dates.

## Examples

``` r
on_weekends <- weekly() %>%
  recur_on_weekends()

# New Year's Day, observed on the nearest weekday if it falls on a weekend
on_new_years <- hol_new_years_day() %>%
  hol_observe(on_weekends, adj_nearest)

# Christmas, observed on the nearest weekday if it falls on a weekend
on_christmas <- hol_christmas() %>%
  hol_observe(on_weekends, adj_nearest)

cal <- rcalendar(on_new_years, on_christmas)
cal
#> <rcalendar[2]>
#> • New Year's Day
#> • Christmas

# In 2010, Christmas fell on a Saturday and was adjusted backwards
cal_events(cal, year = 2010)
#>             name       date
#> 1 New Year's Day 2010-01-01
#> 2      Christmas 2010-12-24

# In 2011, New Year's fell on a Saturday and was adjusted backwards.
# Note that the returned date is in 2010, even though we requested holidays
# for 2011, because most people would consider the actual New Year's date of
# 2011-01-01 part of the 2011 set of holidays, even though it was observed in
# 2010.
cal_events(cal, year = 2011)
#>             name       date
#> 1 New Year's Day 2010-12-31
#> 2      Christmas 2011-12-26

# If you want to filter by the observed date, set `observed = TRUE`, which
# will move the New Year's Day that was observed in 2010 to the 2010 result
cal_events(cal, year = 2010, observed = TRUE)
#>             name       date
#> 1 New Year's Day 2010-01-01
#> 2      Christmas 2010-12-24
#> 3 New Year's Day 2010-12-31
cal_events(cal, year = 2011, observed = TRUE)
#>        name       date
#> 1 Christmas 2011-12-26
```
