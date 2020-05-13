
<!-- README.md is generated from README.Rmd. Please edit that file -->

# almanac

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/almanac/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/almanac?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/DavisVaughan/almanac/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/almanac/actions)
<!-- badges: end -->

``` r
library(almanac)
```

almanac provides tools for working with recurrence rules, the
fundamental building blocks used to identify calendar “events”, such as
weekends or holidays.

## Installation

Install the released version of almanac from CRAN with:

``` r
install.packages("almanac")
```

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("DavisVaughan/almanac")
```

Mac and Windows users should not have any problems installing almanac.
Linux users need libv8 to install the dependency R package, V8. See the
[V8 installation
instructions](https://github.com/jeroen/V8#debian--ubuntu) for more
information. almanac uses ES5 JavaScript, so it does *not* require any
“modern” JavaScript features and should work with the “old” V8 engine
provided by Ubuntu versions before 19.04.

## Recurrence Rules

Constructing recurrence rules looks like this:

``` r
# Thanksgiving = "The fourth Thursday in November"
on_thanksgiving <- yearly() %>% 
  recur_on_ymonth("November") %>%
  recur_on_wday("Thursday", nth = 4)

on_thanksgiving
#> <rrule[yearly / 1900-01-01 / 2100-01-01]>
#> - ymonth: Nov
#> - wday: Thu[4]
```

After constructing a recurrence rule, it can be used to generate dates
that are in the “event set”. For example, you can search for all
Thanksgivings between 2000-2006.

``` r
alma_search("2000-01-01", "2006-12-31", on_thanksgiving)
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

## Recurrence Bundles

The above example just scratches the surface of what almanac can do.
Practically speaking, you’ll probably have multiple holidays and events
that you’d like to combine into one big recurrence object. This is known
as a *recurrence bundle*.

This example creates recurrence rules for weekends and Christmas, and
bundles them together along with the Thanksgiving rule in such a way
that we get the *union* of the underlying event sets.

``` r
on_weekends <- weekly() %>%
  recur_on_weekends()

on_christmas <- yearly() %>%
  recur_on_mday(25) %>%
  recur_on_ymonth("Dec")

bundle <- runion() %>%
  add_rschedule(on_weekends) %>%
  add_rschedule(on_christmas) %>%
  add_rschedule(on_thanksgiving)

bundle
#> <runion[3 rschedules / 0 rdates / 0 exdates]>
```

We can create a stepper that steps over all of the events in the bundle.
If these two holidays were the only ones that your company celebrated,
the stepper could be viewed as a way to step forward by a “business
day”.

For example, Christmas was on a Monday in 2006. If you wanted to step 1
business day forward from the Friday before Christmas, you’d probably
like it to step over the weekend and the Christmas Monday to Tuesday.
The `bundle` lets you do exactly that\!

``` r
business_day <- stepper(bundle)

# Christmas was on a Monday in 2006.
# This is the Friday before Christmas
friday <- as.Date("2006-12-22")

# Step forward 1 business day, going over the weekend and Christmas
friday %s+% business_day(1)
#> [1] "2006-12-26"
```

## Learning More

View the vignettes on [the
website](https://davisvaughan.github.io/almanac/index.html) to learn
more about how to use almanac.

  - `vignette("almanac")`

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
[busday\_offset()](https://docs.scipy.org/doc/numpy/reference/generated/numpy.busday_offset.html).

The author of [gs](https://github.com/jameslairdsmith/gs), James
Laird-Smith, has been a great collaborator as we have bounced ideas off
of each other. gs attempts to solve a similar problem, but with a
slightly different implementation.
