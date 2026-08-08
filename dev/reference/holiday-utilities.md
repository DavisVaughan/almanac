# Holiday utility functions

These three functions allow you to tweak existing holidays created by
[`rholiday()`](rholiday.md) so that they more properly align with
business calendars. The resulting holidays can then be added into an
[`rcalendar()`](rcalendar.md).

- `hol_observe()` adjusts a holiday based on when it is actually
  observed. For example, many holidays that occur on a Saturday are
  actually observed on the preceding Friday or following Monday.

- `hol_offset()` creates a new holiday by *offsetting* it from an
  existing one. For example, Boxing Day is the day after Christmas, and
  the observance of Boxing Day may be dependent on the observance of
  Christmas (i.e. if Christmas is Sunday, it may be observed on Monday,
  so Boxing Day would be observed on Tuesday).

- `hol_rename()` renames an existing holiday. This is typically useful
  after a call to `hol_offset()`, since it doesn't rename the holiday
  but you may want to give it a different name.

## Usage

``` r
hol_observe(x, adjust_on, adjustment)

hol_offset(x, by)

hol_rename(x, name)
```

## Arguments

- x:

  `[rholiday]`

  An rholiday.

- adjust_on:

  `[rschedule]`

  An rschedule that determines when the `adjustment` is to be applied.

- adjustment:

  `[function]`

  An adjustment function to apply to problematic dates. Typically one of
  the pre-existing adjustment functions, like
  [`adj_nearest()`](adjustments.md).

  A custom adjustment function must have two arguments `x` and
  `rschedule`. `x` is the complete vector of dates that possibly need
  adjustment. `rschedule` is the rschedule who's event set determines
  when an adjustment needs to be applied. The function should adjust `x`
  as required and return the adjusted Date vector.

- by:

  `[integer(1)]`

  A single integer to offset by.

- name:

  `[character(1)]`

  A new name for the holiday.

## Examples

``` r

on_weekends <- weekly() %>%
  recur_on_weekends()

# Christmas, adjusted to nearest Friday or Monday if it falls on a weekend
on_christmas <- hol_christmas() %>%
  hol_observe(on_weekends, adj_nearest)

# Boxing Day is the day after Christmas.
# If observed Christmas is a Friday, then observed Boxing Day should be Monday.
# If observed Christmas is a Monday, then observed Boxing Day should be Tuesday.
on_boxing_day <- on_christmas %>%
  hol_offset(1) %>%
  hol_observe(on_weekends, adj_following) %>%
  hol_rename("Boxing Day")

christmas_dates <- alma_events(on_christmas, year = 2010:2015)
boxing_day_dates <- alma_events(on_boxing_day, year = 2010:2015)

data.frame(
  christmas = christmas_dates,
  boxing_day = boxing_day_dates,
  christmas_weekday = lubridate::wday(christmas_dates, label = TRUE),
  boxing_day_weekday = lubridate::wday(boxing_day_dates, label = TRUE)
)
#>    christmas boxing_day christmas_weekday boxing_day_weekday
#> 1 2010-12-24 2010-12-27               Fri                Mon
#> 2 2011-12-26 2011-12-27               Mon                Tue
#> 3 2012-12-25 2012-12-26               Tue                Wed
#> 4 2013-12-25 2013-12-26               Wed                Thu
#> 5 2014-12-25 2014-12-26               Thu                Fri
#> 6 2015-12-25 2015-12-28               Fri                Mon
```
