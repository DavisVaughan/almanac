# Holidays

This page lists a number of pre-created holidays that can be added to a
calendar created with [`rcalendar()`](rcalendar.md). This list makes no
attempt to be comprehensive. If you need to create your own holiday, you
can do so with [`rholiday()`](rholiday.md).

It also makes no attempt to be historically accurate, i.e. Juneteenth
was created in 2021, but `hol_us_juneteenth()` will generate event dates
before that. Because [`rholiday()`](rholiday.md) takes an arbitrary
rschedule object, you can always create an rschedule that is
historically accurate and use that instead.

## Usage

``` r
hol_christmas(since = NULL, until = NULL)

hol_christmas_eve(since = NULL, until = NULL)

hol_easter(since = NULL, until = NULL)

hol_good_friday(since = NULL, until = NULL)

hol_halloween(since = NULL, until = NULL)

hol_new_years_day(since = NULL, until = NULL)

hol_new_years_eve(since = NULL, until = NULL)

hol_st_patricks_day(since = NULL, until = NULL)

hol_valentines_day(since = NULL, until = NULL)

hol_us_election_day(since = NULL, until = NULL)

hol_us_fathers_day(since = NULL, until = NULL)

hol_us_independence_day(since = NULL, until = NULL)

hol_us_indigenous_peoples_day(since = NULL, until = NULL)

hol_us_juneteenth(since = NULL, until = NULL)

hol_us_labor_day(since = NULL, until = NULL)

hol_us_martin_luther_king_junior_day(since = NULL, until = NULL)

hol_us_memorial_day(since = NULL, until = NULL)

hol_us_mothers_day(since = NULL, until = NULL)

hol_us_presidents_day(since = NULL, until = NULL)

hol_us_thanksgiving(since = NULL, until = NULL)

hol_us_veterans_day(since = NULL, until = NULL)
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

## Details

Note that *relative* holidays, such as New Year's Eve, which is 1 day
before New Year's Day, aren't pre-created in a way that allows you to
define observance rules for them that depend on the observance rules of
the holiday they are relative to. If you need to do this, you should
start with the base holiday, here `hol_new_years_day()`, and use
[`hol_observe()`](holiday-utilities.md) and
[`hol_offset()`](holiday-utilities.md) on that to generate a New Year's
Eve holiday that matches your required observance rules. See the
examples of [`hol_offset()`](holiday-utilities.md) for more information.

## Examples

``` r
on_christmas <- hol_christmas()
on_christmas
#> <Christmas>
#>  <rrule>
#>  • frequency: yearly
#>  • range: [1900-01-01, 2100-01-01]
#>  • month of year: Dec
#>  • day of month: 25

# These are like any other rschedule object
alma_events(on_christmas, year = 2020:2025)
#> [1] "2020-12-25" "2021-12-25" "2022-12-25" "2023-12-25" "2024-12-25"
#> [6] "2025-12-25"

# But they can also be added into an rcalendar
cal <- rcalendar(
  on_christmas,
  hol_halloween(),
  hol_new_years_day(),
  hol_us_presidents_day()
)
cal
#> <rcalendar[4]>
#> • Christmas
#> • Halloween
#> • New Year's Day
#> • US Presidents' Day

# Which gives you access to a number of `cal_*()` functions
cal_events(cal, year = 2020:2022)
#>                  name       date
#> 1      New Year's Day 2020-01-01
#> 2  US Presidents' Day 2020-02-17
#> 3           Halloween 2020-10-31
#> 4           Christmas 2020-12-25
#> 5      New Year's Day 2021-01-01
#> 6  US Presidents' Day 2021-02-15
#> 7           Halloween 2021-10-31
#> 8           Christmas 2021-12-25
#> 9      New Year's Day 2022-01-01
#> 10 US Presidents' Day 2022-02-21
#> 11          Halloween 2022-10-31
#> 12          Christmas 2022-12-25
```
