
<!-- README.md is generated from README.Rmd. Please edit that file -->

# almanac

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/almanac/branch/main/graph/badge.svg)](https://app.codecov.io/gh/DavisVaughan/almanac?branch=main)
[![R-CMD-check](https://github.com/DavisVaughan/almanac/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DavisVaughan/almanac/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

``` r
library(almanac)
```

almanac provides tools for working with recurrence rules, the
fundamental building blocks used to identify calendar “events”, such as
weekends or holidays.

Additionally, it provides a full suite of tools for working with
holidays and calendars. It includes a number of built in holidays, such
as `hol_christmas()`, but you can also add your own custom holidays
through `rholiday()`. Once you have a set of holidays specific to your
business, you can aggregate them into a calendar with `rcalendar()`,
which has specialized tooling like `cal_events()` to generate the
holiday dates for a particular year.

## Installation

Install the released version of almanac from CRAN with:

``` r
install.packages("almanac")
```

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("DavisVaughan/almanac")
```

## Recurrence rules

Constructing recurrence rules looks like this:

``` r
# Thanksgiving = "The fourth Thursday in November"
on_thanksgiving <- yearly() %>% 
  recur_on_month_of_year("November") %>%
  recur_on_day_of_week("Thursday", nth = 4)

on_thanksgiving
#> <rrule>
#> • frequency: yearly
#> • range: [1900-01-01, 2100-01-01]
#> • month of year: Nov
#> • day of week: Thu[4]
```

This is the underlying recurrence rule for the Thanksgiving holiday
represented by `hol_us_thanksgiving()`.

After constructing a recurrence rule, it can be used to generate dates
that are in the “event set”. For example, you can search for all
Thanksgivings between 2000-2006.

``` r
alma_events(on_thanksgiving, year = 2000:2006)
#> [1] "2000-11-23" "2001-11-22" "2002-11-28" "2003-11-27" "2004-11-25"
#> [6] "2005-11-24" "2006-11-23"
```

Determine if a particular date is a part of the event set with
`alma_in()`.

``` r
# Is this a Thanksgiving?
alma_in(c("2000-01-01", "2000-11-23"), on_thanksgiving)
#> [1] FALSE  TRUE
```

You can also shift an existing sequence of dates, “stepping over” dates
that are part of the event set.

``` r
wednesday_before_thanksgiving <- as.Date("2000-11-22")

# Thanksgiving was on 2000-11-23.
# This steps over Thanksgiving to 2000-11-24.
# Then steps 1 more day to 2000-11-25.
alma_step(wednesday_before_thanksgiving, n = 2, on_thanksgiving)
#> [1] "2000-11-25"
```

There is an additional “stepper” object you can create for more
intuitive stepping. Combine it with `%s+%` to perform the same step done
by `alma_step()`. Create a stepper function with `stepper()`, and then
use it by supplying the number of days to step.

``` r
step_over_thanksgiving <- stepper(on_thanksgiving)
wednesday_before_thanksgiving %s+% step_over_thanksgiving(2)
#> [1] "2000-11-25"
```

## Holidays and calendars

The above example just scratches the surface of what almanac can do.
Practically speaking, you’ll probably have multiple holidays that you’d
like to combine into one big calendar. almanac provides a full API for
working with holidays and calendars.

This example creates a calendar containing Christmas, Thanksgiving, and
New Year’s Day:

``` r
cal <- rcalendar(
  hol_christmas(),
  hol_us_thanksgiving(),
  hol_new_years_day()
)

cal
#> <rcalendar[3]>
#> • Christmas
#> • US Thanksgiving
#> • New Year's Day
```

We can ask for the next upcoming holiday with `cal_next()`:

``` r
x <- as.Date(c("2019-12-05", "2020-02-05"))
cal_next(x, cal)
#>              name       date
#> 1       Christmas 2019-12-25
#> 2 US Thanksgiving 2020-11-26
```

Or for holidays that belong to a particular year with `cal_events()`:

``` r
events <- cal_events(cal, year = 2028)
events$weekday <- lubridate::wday(events$date, label = TRUE)
events
#>              name       date weekday
#> 1  New Year's Day 2028-01-01     Sat
#> 2 US Thanksgiving 2028-11-23     Thu
#> 3       Christmas 2028-12-25     Mon
```

Note that New Year’s Day occurred on Saturday. If your business
*observes* New Year’s Day on the nearest weekday, you can adjust the
holiday to respect that observance rule before adding it into the
calendar:

``` r
on_weekends <- weekly() %>%
  recur_on_weekends()

cal <- rcalendar(
  hol_christmas(),
  hol_us_thanksgiving(),
  hol_observe(
    hol_new_years_day(), 
    adjust_on = on_weekends, 
    adjustment = adj_nearest
  )
)

# Now it returns the previous Friday for the observed New Year's date.
# Note that this fell in 2027, but was included in the 2028 set of dates
# since most people would consider that part of the 2028 holiday calendar.
events <- cal_events(cal, year = 2028)
events$weekday <- lubridate::wday(events$date, label = TRUE)
events
#>              name       date weekday
#> 1  New Year's Day 2027-12-31     Fri
#> 2 US Thanksgiving 2028-11-23     Thu
#> 3       Christmas 2028-12-25     Mon
```

We can union our calendar with the `on_weekends` rule to get a
recurrence set that represents days when our business is closed. Then we
can create a stepper out of that so we can step forwards by “a business
day.”

``` r
business_day <- stepper(runion(cal, on_weekends))
```

For example, Christmas was on a Monday in 2006. If you wanted to step 1
business day forward from the Friday before Christmas, you’d probably
like it to step over the weekend and the Christmas Monday to finally
land on Tuesday:

``` r
# Christmas was on a Monday in 2006.
# This is the Friday before Christmas
friday <- as.Date("2006-12-22")

# Step forward 1 business day, going over the weekend and Christmas
friday %s+% business_day(1)
#> [1] "2006-12-26"
```

## Learning more

View the vignettes on [the
website](https://davisvaughan.github.io/almanac/index.html) to learn
more about how to use almanac.

- `vignette("almanac")`

- `vignette("holidays-calendars")`

- `vignette("adjust-and-shift")`

- `vignette("quarterly")`

- `vignette("icalendar")`

## Acknowledgements

almanac has developed as a composite of ideas from multiple different
libraries.

First off, it directly embeds the *amazing* JavaScript library
[rrule](https://github.com/jakubroztocil/rrule) for the core event set
calculations. To do this, it uses the equally awesome R package,
[V8](https://github.com/jeroen/V8), from Jeroen Ooms.

The date shifting / adjusting functions are modeled after similar
functions in [QuantLib](https://github.com/lballabio/QuantLib).

The fast binary search based implementations of `alma_next()` and
`alma_step()` are inspired by Pandas and the implementation of Numpy’s
[busday_offset()](https://numpy.org/doc/stable/reference/generated/numpy.busday_offset.html).

The author of [gs](https://github.com/jameslairdsmith/gs), James
Laird-Smith, has been a great collaborator as we have bounced ideas off
of each other. gs attempts to solve a similar problem, but with a
slightly different implementation.
