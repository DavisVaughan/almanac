# Introduction to almanac

This vignette is designed to introduce some of the common terminology
used in almanac to get you up to speed on how to use the package. Along
the way, we will see example usage of a number of the building blocks
that will allow you to construct more complex recurrence objects.

``` r

library(almanac)
```

## Recurrence Rules

A *recurrence rule* is a structured object that determines if a date
should be counted as an *event* or not. At the most basic level, the job
of a recurrence rule is to search through a pre-specified range of dates
and flag any event dates in that range.

To build a recurrence rule, you start with a base recurrence frequency.
There are 4 frequencies to choose from:

- [`daily()`](../reference/rrule.md)

- [`weekly()`](../reference/rrule.md)

- [`monthly()`](../reference/rrule.md)

- [`yearly()`](../reference/rrule.md)

Take the [`yearly()`](../reference/rrule.md) frequency, for example. By
default, this will declare that 1 value per year is an event.

``` r

on_yearly <- yearly()
on_yearly
#> <rrule>
#> • frequency: yearly
#> • range: [1900-01-01, 2100-01-01]
```

The return value of [`yearly()`](../reference/rrule.md) is a *rrule*
object, short for “recurrence rule”. This base object is all you need to
start doing something useful. For example, you can pass this rrule to
[`alma_search()`](../reference/alma_search.md) along with a `from` and
`to` date, and it will return all of the events in that date range.

``` r

alma_search(from = "1990-01-01", to = "1995-12-31", on_yearly)
#> [1] "1990-01-01" "1991-01-01" "1992-01-01" "1993-01-01" "1994-01-01"
#> [6] "1995-01-01"
```

What if we want a yearly value, but we want it on January 5th every
year, rather than on the 1st? [`yearly()`](../reference/rrule.md) has an
important argument called `since` that controls two things: the start
date of the recurrence rule, and information such as the month, or the
day of the month to use if no other conditions have been specified to
override those.

The default of `since` is set to `1900-01-01`, but this is arbitrary
(see [`almanac_since()`](../reference/almanac-defaults.md)). It is
because of this default that in the above example with
[`alma_search()`](../reference/alma_search.md) we get values on January
1st. Let’s change that.

``` r

on_yearly_jan_5 <- yearly(since = "1990-01-05")

alma_search("1990-01-01", "1995-12-31", on_yearly_jan_5)
#> [1] "1990-01-05" "1991-01-05" "1992-01-05" "1993-01-05" "1994-01-05"
#> [6] "1995-01-05"
```

Now that the `since` date has been set to 1990, if we try and find
yearly dates before 1990, they will not be included.

``` r

# Same result as above, because the 1988 and 1989 dates are not included. 
alma_search("1988-01-01", "1995-12-31", on_yearly_jan_5)
#> [1] "1990-01-05" "1991-01-05" "1992-01-05" "1993-01-05" "1994-01-05"
#> [6] "1995-01-05"
```

There is also an `until` argument to [`yearly()`](../reference/rrule.md)
that controls the upper bound of the range to look in. This is
arbitrarily set to `2100-01-01`, but can be expanded or contracted as
required (see [`almanac_until()`](../reference/almanac-defaults.md)).

## Event Set

I mentioned earlier that the job of a recurrence rule is to flag dates
in a pre-specified range to be events or not. The dates that are flagged
as events are known as the *event set*.

In the previous example, we used
[`alma_search()`](../reference/alma_search.md) to extract a subset of
dates from the event set that were between `from` and `to`. You can get
the entire event set with
[`alma_events()`](../reference/alma_events.md). Notice that this is
bounded by our custom `since` date, and the default `until` upper bound.
Otherwise we’d have an infinite event set, which is nice in theory but
bad in practice.

``` r

alma_events(on_yearly_jan_5)
#>   [1] "1990-01-05" "1991-01-05" "1992-01-05" "1993-01-05" "1994-01-05"
#>   [6] "1995-01-05" "1996-01-05" "1997-01-05" "1998-01-05" "1999-01-05"
#>  [11] "2000-01-05" "2001-01-05" "2002-01-05" "2003-01-05" "2004-01-05"
#>  [16] "2005-01-05" "2006-01-05" "2007-01-05" "2008-01-05" "2009-01-05"
#>  [21] "2010-01-05" "2011-01-05" "2012-01-05" "2013-01-05" "2014-01-05"
#>  [26] "2015-01-05" "2016-01-05" "2017-01-05" "2018-01-05" "2019-01-05"
#>  [31] "2020-01-05" "2021-01-05" "2022-01-05" "2023-01-05" "2024-01-05"
#>  [36] "2025-01-05" "2026-01-05" "2027-01-05" "2028-01-05" "2029-01-05"
#>  [41] "2030-01-05" "2031-01-05" "2032-01-05" "2033-01-05" "2034-01-05"
#>  [46] "2035-01-05" "2036-01-05" "2037-01-05" "2038-01-05" "2039-01-05"
#>  [51] "2040-01-05" "2041-01-05" "2042-01-05" "2043-01-05" "2044-01-05"
#>  [56] "2045-01-05" "2046-01-05" "2047-01-05" "2048-01-05" "2049-01-05"
#>  [61] "2050-01-05" "2051-01-05" "2052-01-05" "2053-01-05" "2054-01-05"
#>  [66] "2055-01-05" "2056-01-05" "2057-01-05" "2058-01-05" "2059-01-05"
#>  [71] "2060-01-05" "2061-01-05" "2062-01-05" "2063-01-05" "2064-01-05"
#>  [76] "2065-01-05" "2066-01-05" "2067-01-05" "2068-01-05" "2069-01-05"
#>  [81] "2070-01-05" "2071-01-05" "2072-01-05" "2073-01-05" "2074-01-05"
#>  [86] "2075-01-05" "2076-01-05" "2077-01-05" "2078-01-05" "2079-01-05"
#>  [91] "2080-01-05" "2081-01-05" "2082-01-05" "2083-01-05" "2084-01-05"
#>  [96] "2085-01-05" "2086-01-05" "2087-01-05" "2088-01-05" "2089-01-05"
#> [101] "2090-01-05" "2091-01-05" "2092-01-05" "2093-01-05" "2094-01-05"
#> [106] "2095-01-05" "2096-01-05" "2097-01-05" "2098-01-05" "2099-01-05"
```

You can also check if an existing date is included in a recurrence
rule’s event set with [`alma_in()`](../reference/alma_in.md).

``` r

# Uses the 10th of the month, pulled from `since`
on_monthly <- monthly(since = "1990-01-10")

x <- as.Date("2000-01-08") + 0:5
x
#> [1] "2000-01-08" "2000-01-09" "2000-01-10" "2000-01-11" "2000-01-12"
#> [6] "2000-01-13"

x_in_set <- alma_in(x, on_monthly)
x_in_set
#> [1] FALSE FALSE  TRUE FALSE FALSE FALSE

x[x_in_set]
#> [1] "2000-01-10"
```

### Caching

almanac attempts to be smart by caching the event set of a recurrence
rule the first time that it is queried. This means that the first usage
of a recurrence rule is generally slower than repeated uses.

``` r

since <- "1990-01-01"

on_weekly <- weekly(since = since)

# The first time is "slow"
system.time(alma_search(since, "2000-01-01", on_weekly))
#>    user  system elapsed 
#>   0.104   0.001   0.062

# Repeated access is fast
system.time(alma_search(since, "2000-01-01", on_weekly))
#>    user  system elapsed 
#>       0       0       0

# The entire event set is cached, so even if you change the arguments,
# the operation is still fast.
system.time(alma_search(since, "1990-05-01", on_weekly))
#>    user  system elapsed 
#>       0       0       0
```

## Recurrence Conditions

So far we have worked with the base recurrence rules. Things get *much*
more interesting when we start adding extra conditions to these rules.
Conditions are ways to *limit* or *expand* a given recurrence rule to
hone in on recurring dates that you are particularly interested in. All
condition functions in almanac start with `recur_*()`. For example,
let’s take a monthly rule, which defaults to give us 1 day per month,
and *expand* it to give us every 4th and 16th day of the month.

``` r

on_4th_and_16th <- monthly(since = "2000-01-01") %>%
  recur_on_day_of_month(c(4, 16))

alma_search("2000-01-01", "2000-06-01", on_4th_and_16th)
#>  [1] "2000-01-04" "2000-01-16" "2000-02-04" "2000-02-16" "2000-03-04"
#>  [6] "2000-03-16" "2000-04-04" "2000-04-16" "2000-05-04" "2000-05-16"
```

An important thing to note here is that even though our `since` date is
on the first of the month, we are “overriding” that with the recurrence
condition, so that information is not used.

Recurrence rules can continually be added to further refine your rule.
When you add a condition to a rule, you get another rule back. Let’s try
creating a rule for the recurring holiday, Labor Day. This recurs on the
first Monday of September, yearly. To do this, we will:

- Use a [`yearly()`](../reference/rrule.md) base since this happens 1
  time per year.

- Use
  [`recur_on_month_of_year()`](../reference/recur_on_month_of_year.md)
  to hone in on September.

- Use [`recur_on_day_of_week()`](../reference/recur_on_day_of_week.md)
  to hone in on the first Monday of the month.

``` r

on_labor_day <- yearly() %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_week("Monday", nth = 1)

alma_search("2000-01-01", "2005-01-01", on_labor_day)
#> [1] "2000-09-04" "2001-09-03" "2002-09-02" "2003-09-01" "2004-09-06"
```

The `nth` argument of
[`recur_on_day_of_week()`](../reference/recur_on_day_of_week.md) is
especially useful for selecting from the end of the month. If we wanted
the *last* Monday in September instead, we could do:

``` r

on_last_monday_in_sept <- yearly(since = "2000-01-01") %>%
  recur_on_month_of_year("Sep") %>%
  recur_on_day_of_week("Monday", nth = -1)

alma_search("2000-01-01", "2005-01-01", on_last_monday_in_sept)
#> [1] "2000-09-25" "2001-09-24" "2002-09-30" "2003-09-29" "2004-09-27"
```

## Recurrence Sets

Recurrence rules are powerful tools on their own, but they aren’t enough
to solve every task. Say you want to construct a rule that includes both
Christmas and Labor Day as events. It would be impossible to construct
this kind of event set using a single rule, but if you could *bundle*
multiple rules together, one for Christmas and one for Labor Day, then
it would be possible.

An *rset* is a bundle of recurrence schedules. A recurrence schedule, or
rschedule, is an overarching term for both rrules and rsets. There are
three types of rsets in almanac. Each create their event set by
performing some kind of set operation on the event sets of the
underlying rschedules that you added to the set.

- [`runion()`](../reference/rset.md) takes the union.

- [`rintersect()`](../reference/rset.md) takes the intersection.

- [`rsetdiff()`](../reference/rset.md) takes the set difference.

The most useful rset is runion, as this allows you construct an event
set that, for example, falls on multiple holidays and all weekends. The
following creates an runion from rrules based on Christmas and Labor
Day.

``` r

on_christmas <- yearly() %>%
  recur_on_month_of_year("Dec") %>%
  recur_on_day_of_month(25)

christmas_or_labor_day <- runion(
  on_christmas,
  on_labor_day
)

alma_search("2000-01-01", "2002-01-01", christmas_or_labor_day)
#> [1] "2000-09-04" "2000-12-25" "2001-09-03" "2001-12-25"

christmas_or_labor_day_except_2000_labor_day <- rsetdiff(
  christmas_or_labor_day, 
  rcustom("2000-09-04")
)

alma_search("2000-01-01", "2002-01-01", christmas_or_labor_day_except_2000_labor_day)
#> [1] "2000-12-25" "2001-09-03" "2001-12-25"
```

A recurrence set is a critical data structure in almanac. It serves as a
general container to dump all of your company’s holiday and weekend
recurrence rules.
