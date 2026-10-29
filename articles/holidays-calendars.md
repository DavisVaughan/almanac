# Holidays and calendars

``` r

library(almanac)

# A rule for weekends
on_weekends <- weekly() %>%
  recur_on_weekends()
```

almanac provides low-level tools for building all manner of recurrence
rules, but it also provides a fairly high-level set of tools for working
with holidays and calendars. These are arguably the most useful part of
almanac, because they do a lot of the heavy lifting of defining your own
holidays and observance rules for you.

## Holidays

There are a number of pre-generated holidays in almanac, which all start
with `hol_*()`, such as [`hol_christmas()`](../reference/holidays.md).
If the holiday is specific to a country, then it is also prefixed as
such, like [`hol_us_veterans_day()`](../reference/holidays.md)
(currently only global and US holidays are provided).

A holiday object is just another type of recurrence schedule:

``` r

on_christmas <- hol_christmas()
on_christmas
#> <Christmas>
#>  <rrule>
#>  • frequency: yearly
#>  • range: [1900-01-01, 2100-01-01]
#>  • month of year: Dec
#>  • day of month: 25
```

This means that you can use it with any of the `alma_*()` functions you
might have learned about in the other vignettes.

``` r

alma_events(on_christmas, year = 2020:2025)
#> [1] "2020-12-25" "2021-12-25" "2022-12-25" "2023-12-25" "2024-12-25"
#> [6] "2025-12-25"
```

You’ll use these holiday objects to build up a *calendar*, which is a
bundle of individual holidays that represent the rules of your specific
business.

There are 3 helpers that are specific to holidays:

- [`hol_observe()`](../reference/holiday-utilities.md)

- [`hol_offset()`](../reference/holiday-utilities.md)

- [`hol_rename()`](../reference/holiday-utilities.md)

### `hol_observe()`

[`hol_observe()`](../reference/holiday-utilities.md) adjusts a holiday
to respect observance rules that might be set by your company. For
example, if Christmas falls on Saturday, then your company may actually
*observe* it on the preceding Friday or following Monday.

``` r

on_christmas <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest)

on_christmas
#> <Christmas>
#>  <radjusted>
#>   adjust:
#>   <rrule>
#>   • frequency: yearly
#>   • range: [1900-01-01, 2100-01-01]
#>   • month of year: Dec
#>   • day of month: 25
#>   adjust on:
#>   <rrule>
#>   • frequency: weekly
#>   • range: [1900-01-01, 2100-01-01]
#>   • day of week: Sat and Sun
```

If you’ve read
[`vignette("adjust-and-shift")`](../articles/adjust-and-shift.md), then
you’ll recognize that observances look similar to
[`radjusted()`](../reference/radjusted.md), and in fact that is the
tooling that is used under the hood.

If we run our updated holiday through
[`alma_events()`](../reference/alma_events.md) again, you’ll see that it
now rolls to the nearest weekday if Christmas falls on a weekend.

``` r

alma_events(on_christmas, year = 2020:2025)
#> [1] "2020-12-25" "2021-12-24" "2022-12-26" "2023-12-25" "2024-12-25"
#> [6] "2025-12-25"
```

### `hol_offset()`

[`hol_offset()`](../reference/holiday-utilities.md) allows you to create
holidays that are *relative* to some other holiday. For example, Good
Friday is always the Friday before Easter.

Let’s take a look at building a holiday for Boxing Day, the day after
Christmas. [`hol_offset()`](../reference/holiday-utilities.md) retains
the name of the original holiday, so we’ll also need
[`hol_rename()`](../reference/holiday-utilities.md) to rename it to
Boxing Day:

``` r

on_boxing_day <- hol_christmas() %>%
  hol_offset(by = 1) %>%
  hol_rename("Boxing Day")

on_boxing_day
#> <Boxing Day>
#>  <roffset[by = 1]>
#>   <rrule>
#>   • frequency: yearly
#>   • range: [1900-01-01, 2100-01-01]
#>   • month of year: Dec
#>   • day of month: 25
```

``` r

alma_events(on_boxing_day, year = 2020:2025)
#> [1] "2020-12-26" "2021-12-26" "2022-12-26" "2023-12-26" "2024-12-26"
#> [6] "2025-12-26"
```

Great! Now what if we wanted to add an observance rule to Boxing Day?
This is actually pretty complicated, because it is dependent on the
observance rules for Christmas. For example, if Christmas falls on a
Saturday, but is observed on Friday, then we probably want Boxing Day to
be observed on the following Monday. If Christmas falls on a Sunday and
is observed on the following Monday, then Boxing Day would be observed
on the following Tuesday. Luckily you can layer
[`hol_observe()`](../reference/holiday-utilities.md) with
[`hol_offset()`](../reference/holiday-utilities.md) to build up a
recurrence schedule that matches the rules we are looking for:

``` r

on_christmas <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest)

on_boxing_day <- hol_christmas() %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_nearest) %>%
  hol_offset(by = 1) %>%
  hol_observe(adjust_on = on_weekends, adjustment = adj_following) %>%
  hol_rename("Boxing Day")
```

``` r

df <- data.frame(
  christmas = alma_events(on_christmas, year = 2020:2025),
  boxing_day = alma_events(on_boxing_day, year = 2020:2025)
)
df$christmas_weekday <- lubridate::wday(df$christmas, label = TRUE)
df$boxing_day_weekday <- lubridate::wday(df$boxing_day, label = TRUE)

df
#>    christmas boxing_day christmas_weekday boxing_day_weekday
#> 1 2020-12-25 2020-12-28               Fri                Mon
#> 2 2021-12-24 2021-12-27               Fri                Mon
#> 3 2022-12-26 2022-12-27               Mon                Tue
#> 4 2023-12-25 2023-12-26               Mon                Tue
#> 5 2024-12-25 2024-12-26               Wed                Thu
#> 6 2025-12-25 2025-12-26               Thu                Fri
```

### Custom holidays

almanac doesn’t attempt to provide a comprehensive set of holidays.
Instead, it makes it easy for you to create your own using
[`rholiday()`](../reference/rholiday.md). All you need is a recurrence
rule that aligns with the holiday date and a name. Let’s create one for
Canada Day, which occurs on July 1st each year.

``` r

hol_canada_day <- function(since = NULL, until = NULL) {
  out <- yearly(since = since, until = until)
  out <- recur_on_month_of_year(out, "July")
  out <- recur_on_day_of_month(out, 1L)
  
  rholiday(rschedule = out, name = "Canada Day")
}
```

``` r

hol_canada_day()
#> <Canada Day>
#>  <rrule>
#>  • frequency: yearly
#>  • range: [1900-01-01, 2100-01-01]
#>  • month of year: Jul
#>  • day of month: 1

alma_next(as.Date("2019-01-01"), hol_canada_day())
#> [1] "2019-07-01"
```

## Calendars

Holidays are great for one off operations, but typically if you are
building a business calendar then you’ll need to bundle multiple
holidays together. To do that, you’ll need a *calendar* object. You can
create one by providing holidays to
[`rcalendar()`](../reference/rcalendar.md):

``` r

cal <- rcalendar(
  hol_christmas(),
  hol_new_years_day(),
  hol_canada_day()
)

cal
#> <rcalendar[3]>
#> • Christmas
#> • New Year's Day
#> • Canada Day
```

Like holidays, calendars are recurrence schedules that work with any of
the `alma_*()` functions, but they also have their own special `cal_*()`
API.

### `cal_events()`

If you’d like to generate the holidays for a particular year, then you
should use `cal_events():`

``` r

cal_events(cal, year = 2023)
#>             name       date
#> 1 New Year's Day 2023-01-01
#> 2     Canada Day 2023-07-01
#> 3      Christmas 2023-12-25
```

This is similar to [`alma_events()`](../reference/alma_events.md), but
there are two big differences:

- The holiday name is displayed along with the event date

- It has special support for observance rules

Note that we didn’t add any observance rules into our original calendar,
let’s go back and adjust our holidays to roll off weekends:

``` r

cal <- rcalendar(
  hol_christmas() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
  hol_new_years_day() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_nearest),
  # Canada normally rolls their holidays forward to the following Monday
  hol_canada_day() %>%
    hol_observe(adjust_on = on_weekends, adjustment = adj_following)
)
```

Note that the holidays generated by
[`cal_events()`](../reference/cal_events.md) are different from the
previous ones because they now respect the observance rules when a
holiday falls on a weekend.

``` r

cal_events(cal, year = 2023)
#>             name       date
#> 1 New Year's Day 2023-01-02
#> 2     Canada Day 2023-07-03
#> 3      Christmas 2023-12-25
```

There is one more observance related feature worth pointing out. Let’s
take a look at holidays for 2011:

``` r

cal_events(cal, year = 2011)
#>             name       date
#> 1 New Year's Day 2010-12-31
#> 2     Canada Day 2011-07-01
#> 3      Christmas 2011-12-26
```

New Year’s Day fell on a Saturday that year, so it was adjusted
*backwards* to the preceding Friday, which actually fell in 2010.
[`cal_events()`](../reference/cal_events.md) is smart enough to know
that if you are requesting “2011’s holidays,” then New Year’s Day should
probably be included even if it was *observed* in a different year.

If you don’t want this behavior, you can set `observed = TRUE` to use
the observed year when filtering for the `year`.

``` r

# New Year's Day is gone
cal_events(cal, year = 2011, observed = TRUE)
#>         name       date
#> 1 Canada Day 2011-07-01
#> 2  Christmas 2011-12-26

# And is now listed twice here
cal_events(cal, year = 2010, observed = TRUE)
#>             name       date
#> 1 New Year's Day 2010-01-01
#> 2     Canada Day 2010-07-01
#> 3      Christmas 2010-12-24
#> 4 New Year's Day 2010-12-31
```

### `cal_match()`

If you’ve ever needed to determine if a date is a holiday, then you’ve
likely reached for [`alma_in()`](../reference/alma_in.md), which works
like `%in%` and returns a logical vector. But if you also need to
determine *which* holiday that date corresponded to, then you’ll need to
use [`cal_match()`](../reference/cal_match.md) instead:

``` r

x <- as.Date(c(
  "2019-12-25",
  "2019-12-26",
  "2010-12-31",
  "2011-01-01"
))

data.frame(
  x = x,
  name = cal_match(x, cal)
)
#>            x           name
#> 1 2019-12-25      Christmas
#> 2 2019-12-26           <NA>
#> 3 2010-12-31 New Year's Day
#> 4 2011-01-01           <NA>
```

Note that because our calendar has observance rules baked in, it shows
`2010-12-31` (a Friday) as New Year’s Day rather than the 1st of the
year (a Saturday).

### US federal calendar

almanac comes with one pre-built calendar,
[`cal_us_federal()`](../reference/cal_us_federal.md), which contains the
federally recognized holidays in the United States. It is meant to serve
as an example of what you can build with almanac, but it is also pretty
useful on its own:

``` r

cal_us_federal()
#> <rcalendar[11]>
#> • New Year's Day
#> • US Martin Luther King Jr. Day
#> • US Presidents' Day
#> • US Memorial Day
#> • US Juneteenth
#> • US Independence Day
#> • US Labor Day
#> • US Indigenous Peoples' Day
#> • US Veterans Day
#> • US Thanksgiving
#> • Christmas
```

``` r

cal_events(cal_us_federal(), year = 2023)
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
```

Note that this calendar doesn’t claim to be historically accurate, it is
only intended to be a representation of the current federally recognized
holidays. You *can* build historically accurate calendars (i.e. you can
represent that Juneteenth wasn’t celebrated before 2021) it just takes a
bit more effort.
