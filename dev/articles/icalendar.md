# iCalendar specification

The Internet Engineering Task Force (IETF) has released an *exhaustive*
guide about how to design a calendar application. It is known as the
[iCalendar specification guide,
RFC-5545](https://www.rfc-editor.org/rfc/rfc5545). Recurrence rules are
a small subsection of this, and the JavaScript library underlying
almanac was built to comply with the specification guide. To show that
almanac also complies with (pretty much all) of the guide as well, this
vignette will go through the large list of examples that the guide
provides to ensure that your application is doing the right thing. This
will also serve as a way to show how flexible almanac is to construct a
wide range of recurrence rules.

The specific section of the iCalendar guide that mentions recurrence
rules is
[3.3.10](https://www.rfc-editor.org/rfc/rfc5545#section-3.3.10), and the
section with all of the examples is
[3.8.5.3](https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.3).

For whatever reason, most examples use a start date of `1997-09-02`. To
show the results, for terminating recurrence rules (those that have a
limited count or an until date) I will use
[`alma_search()`](../reference/alma_search.md) with `since` as the
`from` date and a `to` date far enough in the future to show useful
results. For non-terminating rules, I will choose a `to` date that shows
enough to be informative, without being overwhelming.

``` r

library(almanac)

since <- as.Date("1997-09-02")
to <- as.Date("2010-01-01")
```

### Daily for 10 events

``` r

rrule <- daily(since) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-02" "1997-09-03" "1997-09-04" "1997-09-05" "1997-09-06"
#>  [6] "1997-09-07" "1997-09-08" "1997-09-09" "1997-09-10" "1997-09-11"
```

### Daily until 1997-12-24

``` r

rrule <- daily(since, until = "1997-12-24")

alma_search(since, to, rrule)
#>   [1] "1997-09-02" "1997-09-03" "1997-09-04" "1997-09-05" "1997-09-06"
#>   [6] "1997-09-07" "1997-09-08" "1997-09-09" "1997-09-10" "1997-09-11"
#>  [11] "1997-09-12" "1997-09-13" "1997-09-14" "1997-09-15" "1997-09-16"
#>  [16] "1997-09-17" "1997-09-18" "1997-09-19" "1997-09-20" "1997-09-21"
#>  [21] "1997-09-22" "1997-09-23" "1997-09-24" "1997-09-25" "1997-09-26"
#>  [26] "1997-09-27" "1997-09-28" "1997-09-29" "1997-09-30" "1997-10-01"
#>  [31] "1997-10-02" "1997-10-03" "1997-10-04" "1997-10-05" "1997-10-06"
#>  [36] "1997-10-07" "1997-10-08" "1997-10-09" "1997-10-10" "1997-10-11"
#>  [41] "1997-10-12" "1997-10-13" "1997-10-14" "1997-10-15" "1997-10-16"
#>  [46] "1997-10-17" "1997-10-18" "1997-10-19" "1997-10-20" "1997-10-21"
#>  [51] "1997-10-22" "1997-10-23" "1997-10-24" "1997-10-25" "1997-10-26"
#>  [56] "1997-10-27" "1997-10-28" "1997-10-29" "1997-10-30" "1997-10-31"
#>  [61] "1997-11-01" "1997-11-02" "1997-11-03" "1997-11-04" "1997-11-05"
#>  [66] "1997-11-06" "1997-11-07" "1997-11-08" "1997-11-09" "1997-11-10"
#>  [71] "1997-11-11" "1997-11-12" "1997-11-13" "1997-11-14" "1997-11-15"
#>  [76] "1997-11-16" "1997-11-17" "1997-11-18" "1997-11-19" "1997-11-20"
#>  [81] "1997-11-21" "1997-11-22" "1997-11-23" "1997-11-24" "1997-11-25"
#>  [86] "1997-11-26" "1997-11-27" "1997-11-28" "1997-11-29" "1997-11-30"
#>  [91] "1997-12-01" "1997-12-02" "1997-12-03" "1997-12-04" "1997-12-05"
#>  [96] "1997-12-06" "1997-12-07" "1997-12-08" "1997-12-09" "1997-12-10"
#> [101] "1997-12-11" "1997-12-12" "1997-12-13" "1997-12-14" "1997-12-15"
#> [106] "1997-12-16" "1997-12-17" "1997-12-18" "1997-12-19" "1997-12-20"
#> [111] "1997-12-21" "1997-12-22" "1997-12-23" "1997-12-24"
```

### Every other day, forever

``` r

rrule <- daily(since) %>%
  recur_on_interval(2)

alma_search(since, "1997-10-01", rrule)
#>  [1] "1997-09-02" "1997-09-04" "1997-09-06" "1997-09-08" "1997-09-10"
#>  [6] "1997-09-12" "1997-09-14" "1997-09-16" "1997-09-18" "1997-09-20"
#> [11] "1997-09-22" "1997-09-24" "1997-09-26" "1997-09-28" "1997-09-30"
```

### Every 10 days, 5 events

``` r

rrule <- daily(since) %>%
  recur_on_interval(10) %>%
  recur_for_count(5)

alma_search(since, to, rrule)
#> [1] "1997-09-02" "1997-09-12" "1997-09-22" "1997-10-02" "1997-10-12"
```

### Every day in January, for 3 years

``` r

rrule <- daily(since = "1998-01-01", until = "2000-01-31") %>%
  recur_on_month_of_year("January")

alma_search(since, to, rrule)
#>  [1] "1998-01-01" "1998-01-02" "1998-01-03" "1998-01-04" "1998-01-05"
#>  [6] "1998-01-06" "1998-01-07" "1998-01-08" "1998-01-09" "1998-01-10"
#> [11] "1998-01-11" "1998-01-12" "1998-01-13" "1998-01-14" "1998-01-15"
#> [16] "1998-01-16" "1998-01-17" "1998-01-18" "1998-01-19" "1998-01-20"
#> [21] "1998-01-21" "1998-01-22" "1998-01-23" "1998-01-24" "1998-01-25"
#> [26] "1998-01-26" "1998-01-27" "1998-01-28" "1998-01-29" "1998-01-30"
#> [31] "1998-01-31" "1999-01-01" "1999-01-02" "1999-01-03" "1999-01-04"
#> [36] "1999-01-05" "1999-01-06" "1999-01-07" "1999-01-08" "1999-01-09"
#> [41] "1999-01-10" "1999-01-11" "1999-01-12" "1999-01-13" "1999-01-14"
#> [46] "1999-01-15" "1999-01-16" "1999-01-17" "1999-01-18" "1999-01-19"
#> [51] "1999-01-20" "1999-01-21" "1999-01-22" "1999-01-23" "1999-01-24"
#> [56] "1999-01-25" "1999-01-26" "1999-01-27" "1999-01-28" "1999-01-29"
#> [61] "1999-01-30" "1999-01-31" "2000-01-01" "2000-01-02" "2000-01-03"
#> [66] "2000-01-04" "2000-01-05" "2000-01-06" "2000-01-07" "2000-01-08"
#> [71] "2000-01-09" "2000-01-10" "2000-01-11" "2000-01-12" "2000-01-13"
#> [76] "2000-01-14" "2000-01-15" "2000-01-16" "2000-01-17" "2000-01-18"
#> [81] "2000-01-19" "2000-01-20" "2000-01-21" "2000-01-22" "2000-01-23"
#> [86] "2000-01-24" "2000-01-25" "2000-01-26" "2000-01-27" "2000-01-28"
#> [91] "2000-01-29" "2000-01-30" "2000-01-31"
```

### Weekly for 10 events

``` r

rrule <- weekly(since) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-02" "1997-09-09" "1997-09-16" "1997-09-23" "1997-09-30"
#>  [6] "1997-10-07" "1997-10-14" "1997-10-21" "1997-10-28" "1997-11-04"
```

### Weekly until December 24, 1997

``` r

rrule <- weekly(since, until = "1997-12-24")

alma_search(since, to, rrule)
#>  [1] "1997-09-02" "1997-09-09" "1997-09-16" "1997-09-23" "1997-09-30"
#>  [6] "1997-10-07" "1997-10-14" "1997-10-21" "1997-10-28" "1997-11-04"
#> [11] "1997-11-11" "1997-11-18" "1997-11-25" "1997-12-02" "1997-12-09"
#> [16] "1997-12-16" "1997-12-23"
```

### Every other week - forever

``` r

rrule <- weekly(since) %>%
  recur_on_interval(2)

alma_search(since, "1997-11-01", rrule)
#> [1] "1997-09-02" "1997-09-16" "1997-09-30" "1997-10-14" "1997-10-28"
```

### Weekly on Tuesday and Thursday for five weeks

``` r

rrule <- weekly(since) %>%
  recur_on_day_of_week(c("Tue", "Thur")) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-02" "1997-09-04" "1997-09-09" "1997-09-11" "1997-09-16"
#>  [6] "1997-09-18" "1997-09-23" "1997-09-25" "1997-09-30" "1997-10-02"
```

### Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997

``` r

rrule <- weekly(since = "1997-09-01", until = "1997-12-24") %>%
  recur_on_day_of_week(c("Mon", "Wed", "Fri")) %>%
  recur_on_interval(2)

alma_search("1997-09-01", to, rrule)
#>  [1] "1997-09-01" "1997-09-03" "1997-09-05" "1997-09-15" "1997-09-17"
#>  [6] "1997-09-19" "1997-09-29" "1997-10-01" "1997-10-03" "1997-10-13"
#> [11] "1997-10-15" "1997-10-17" "1997-10-27" "1997-10-29" "1997-10-31"
#> [16] "1997-11-10" "1997-11-12" "1997-11-14" "1997-11-24" "1997-11-26"
#> [21] "1997-11-28" "1997-12-08" "1997-12-10" "1997-12-12" "1997-12-22"
#> [26] "1997-12-24"
```

### Every other week on Tuesday and Thursday, for 8 events

``` r

rrule <- weekly(since) %>%
  recur_on_day_of_week(c("Tue", "Thu")) %>%
  recur_on_interval(2) %>%
  recur_for_count(8)

alma_search(since, to, rrule)
#> [1] "1997-09-02" "1997-09-04" "1997-09-16" "1997-09-18" "1997-09-30"
#> [6] "1997-10-02" "1997-10-14" "1997-10-16"
```

### Monthly on the first Friday for 10 events

``` r

rrule <- monthly("1997-09-05") %>%
  recur_on_day_of_week("Fri", nth = 1) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-05" "1997-10-03" "1997-11-07" "1997-12-05" "1998-01-02"
#>  [6] "1998-02-06" "1998-03-06" "1998-04-03" "1998-05-01" "1998-06-05"
```

### Monthly on the first Friday until December 24, 1997

``` r

rrule <- monthly(since = "1997-09-05", until = "1997-12-24") %>%
  recur_on_day_of_week("Fri", nth = 1)

alma_search(since, to, rrule)
#> [1] "1997-09-05" "1997-10-03" "1997-11-07" "1997-12-05"
```

### Every other month on the first and last Sunday of the month for 10 events

``` r

rrule <- monthly("1997-09-07") %>%
  recur_on_day_of_week("Sun", nth = c(1, -1)) %>%
  recur_on_interval(2) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-07" "1997-09-28" "1997-11-02" "1997-11-30" "1998-01-04"
#>  [6] "1998-01-25" "1998-03-01" "1998-03-29" "1998-05-03" "1998-05-31"
```

### Monthly on the second-to-last Monday of the month for 6 months

``` r

rrule <- monthly("1997-09-22") %>%
  recur_on_day_of_week("Mon", nth = -2) %>%
  recur_for_count(6)

alma_search(since, to, rrule)
#> [1] "1997-09-22" "1997-10-20" "1997-11-17" "1997-12-22" "1998-01-19"
#> [6] "1998-02-16"
```

### Monthly on the third-to-the-last day of the month, forever

``` r

rrule <- monthly("1997-09-28") %>%
  recur_on_day_of_month(-3)

alma_search(since, "1997-12-01", rrule)
#> [1] "1997-09-28" "1997-10-29" "1997-11-28"
```

### Monthly on the 2nd and 15th of the month for 10 events

``` r

rrule <- monthly("1997-09-02") %>%
  recur_on_day_of_month(c(2, 15)) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-02" "1997-09-15" "1997-10-02" "1997-10-15" "1997-11-02"
#>  [6] "1997-11-15" "1997-12-02" "1997-12-15" "1998-01-02" "1998-01-15"
```

### Monthly on the first and last day of the month for 10 events

``` r

rrule <- monthly("1997-09-30") %>%
  recur_on_day_of_month(c(1, -1)) %>%
  recur_for_count(10)

alma_search(since, to, rrule)
#>  [1] "1997-09-30" "1997-10-01" "1997-10-31" "1997-11-01" "1997-11-30"
#>  [6] "1997-12-01" "1997-12-31" "1998-01-01" "1998-01-31" "1998-02-01"
```

### Every 18 months on the 10th thru 15th of the month for 10 events

``` r

rrule <- monthly("1997-09-10") %>%
  recur_on_interval(18) %>%
  recur_on_day_of_month(10:15) %>%
  recur_for_count(10)

alma_search("1997-09-10", to, rrule)
#>  [1] "1997-09-10" "1997-09-11" "1997-09-12" "1997-09-13" "1997-09-14"
#>  [6] "1997-09-15" "1999-03-10" "1999-03-11" "1999-03-12" "1999-03-13"
```

### Every Tuesday, every other month

``` r

rrule <- monthly(since) %>%
  recur_on_interval(2) %>%
  recur_on_day_of_week("Tuesday")

alma_search(since, "1997-12-01", rrule)
#> [1] "1997-09-02" "1997-09-09" "1997-09-16" "1997-09-23" "1997-09-30"
#> [6] "1997-11-04" "1997-11-11" "1997-11-18" "1997-11-25"
```

### Yearly in June and July for 10 events

Note that the day of the month is taken from the `since` date, since it
is not otherwise specified!

``` r

rrule <- yearly("1997-06-10") %>%
  recur_on_month_of_year(c("June", "July")) %>%
  recur_for_count(10)

alma_search("1997-06-10", to, rrule)
#>  [1] "1997-06-10" "1997-07-10" "1998-06-10" "1998-07-10" "1999-06-10"
#>  [6] "1999-07-10" "2000-06-10" "2000-07-10" "2001-06-10" "2001-07-10"
```

### Every other year on January, February, and March for 10 events

``` r

rrule <- yearly("1997-03-10") %>%
  recur_on_month_of_year(c("Jan", "Feb", "Mar")) %>%
  recur_on_interval(2) %>%
  recur_for_count(10)

alma_search("1997-03-10", to, rrule)
#>  [1] "1997-03-10" "1999-01-10" "1999-02-10" "1999-03-10" "2001-01-10"
#>  [6] "2001-02-10" "2001-03-10" "2003-01-10" "2003-02-10" "2003-03-10"
```

### Every third year on the 1st, 100th, and 200th day for 10 events

``` r

rrule <- yearly("1997-01-01") %>%
  recur_on_day_of_year(c(1, 100, 200)) %>%
  recur_on_interval(3) %>%
  recur_for_count(10)

alma_search("1997-01-01", to, rrule)
#>  [1] "1997-01-01" "1997-04-10" "1997-07-19" "2000-01-01" "2000-04-09"
#>  [6] "2000-07-18" "2003-01-01" "2003-04-10" "2003-07-19" "2006-01-01"
```

### Every 20th Monday of the year, forever

``` r

rrule <- yearly("1997-05-19") %>%
  recur_on_day_of_week("Monday", nth = 20)

alma_search("1997-05-19", "2000-01-01", rrule)
#> [1] "1997-05-19" "1998-05-18" "1999-05-17"
```

### Monday of week number 20 (where the default start of the week is Monday), forever

Default week start in almanac is on Monday.

``` r

rrule <- yearly("1997-05-12") %>%
  recur_on_week_of_year(20) %>%
  recur_on_day_of_week("Monday")

alma_search("1997-05-12", "2000-01-01", rrule)
#> [1] "1997-05-12" "1998-05-11" "1999-05-17"
```

### Every Thursday in March, forever

``` r

rrule <- yearly("1997-03-13") %>%
  recur_on_day_of_week("Thursday") %>%
  recur_on_month_of_year("March")

alma_search("1997-03-13", "2000-01-01", rrule)
#>  [1] "1997-03-13" "1997-03-20" "1997-03-27" "1998-03-05" "1998-03-12"
#>  [6] "1998-03-19" "1998-03-26" "1999-03-04" "1999-03-11" "1999-03-18"
#> [11] "1999-03-25"
```

### Every Thursday, but only during June, July, and August, forever

``` r

rrule <- yearly("1997-06-05") %>%
  recur_on_day_of_week("Thursday") %>%
  recur_on_month_of_year(c("Jun", "July", "Aug"))

alma_search("1997-06-05", "1999-01-01", rrule)
#>  [1] "1997-06-05" "1997-06-12" "1997-06-19" "1997-06-26" "1997-07-03"
#>  [6] "1997-07-10" "1997-07-17" "1997-07-24" "1997-07-31" "1997-08-07"
#> [11] "1997-08-14" "1997-08-21" "1997-08-28" "1998-06-04" "1998-06-11"
#> [16] "1998-06-18" "1998-06-25" "1998-07-02" "1998-07-09" "1998-07-16"
#> [21] "1998-07-23" "1998-07-30" "1998-08-06" "1998-08-13" "1998-08-20"
#> [26] "1998-08-27"
```

### Every Friday the 13th, forever

``` r

rrule <- yearly(since) %>%
  recur_on_day_of_week("Friday") %>%
  recur_on_day_of_month(13)

alma_search(since, "2001-01-01", rrule)
#> [1] "1998-02-13" "1998-03-13" "1998-11-13" "1999-08-13" "2000-10-13"
```

### The first Saturday that follows the first Sunday of the month, forever

``` r

rrule <- yearly("1997-09-13") %>%
  recur_on_day_of_week("Saturday") %>%
  recur_on_day_of_month(7:13)

alma_search("1997-09-13", "1998-06-01", rrule)
#> [1] "1997-09-13" "1997-10-11" "1997-11-08" "1997-12-13" "1998-01-10"
#> [6] "1998-02-07" "1998-03-07" "1998-04-11" "1998-05-09"
```

### Every 4 years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day)

``` r

rrule <- yearly("1996-11-05") %>%
  recur_on_day_of_week("Tuesday") %>%
  recur_on_day_of_month(2:8) %>%
  recur_on_interval(4) %>%
  recur_on_month_of_year("November")

alma_search("1996-11-05", "2010-12-31", rrule)
#> [1] "1996-11-05" "2000-11-07" "2004-11-02" "2008-11-04"
```

### The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months

[`recur_on_position()`](../reference/recur_on_position.md) is an
incredibly powerful tool, allowing you to select a position *within the
frequency*. So this selects the third element of each monthly set, where
the monthly set contains all of the Tuesday, Wednesday, and Thursdays of
the month.

``` r

rrule <- monthly("1997-09-04") %>%
  recur_on_day_of_week(c("Tue", "Wed", "Thu")) %>%
  recur_on_position(3) %>%
  recur_for_count(3)

alma_search("1997-09-04", to, rrule)
#> [1] "1997-09-04" "1997-10-07" "1997-11-06"
```

### The second-to-last weekday of the month

``` r

rrule <- monthly("1997-09-29") %>%
  recur_on_day_of_week(1:5) %>%
  recur_on_position(-2)

alma_search("1997-09-29", "1998-12-01", rrule)
#>  [1] "1997-09-29" "1997-10-30" "1997-11-27" "1997-12-30" "1998-01-29"
#>  [6] "1998-02-26" "1998-03-30" "1998-04-29" "1998-05-28" "1998-06-29"
#> [11] "1998-07-30" "1998-08-28" "1998-09-29" "1998-10-29" "1998-11-27"
```

### An example where the days generated makes a difference because of week_start

``` r

rrule <- weekly("1997-08-05") %>%
  recur_on_interval(2) %>%
  recur_for_count(4) %>%
  recur_on_day_of_week(c("Tue", "Sun")) %>%
  recur_with_week_start("Monday")

# Week 1: 1997-08-04 -> 1997-08-10
# Week 2: 1997-08-11 -> 1997-08-17 (skipped)
# Week 3: 1997-08-18 -> 1997-08-24 
alma_search("1997-08-05", "1998-12-01", rrule)
#> [1] "1997-08-05" "1997-08-10" "1997-08-19" "1997-08-24"
```

Changing the week start to Sunday.

``` r

rrule <- weekly("1997-08-05") %>%
  recur_on_interval(2) %>%
  recur_for_count(4) %>%
  recur_on_day_of_week(c("Tue", "Sun")) %>%
  recur_with_week_start("Sunday")

# Week 1: 1997-08-03 -> 1997-08-09
# Week 2: 1997-08-10 -> 1997-08-16 (skipped)
# Week 3: 1997-08-17 -> 1997-08-23
# Week 4: 1997-08-24 -> 1997-08-30 (skipped)
# Week 5: 1997-08-31 -> 1997-09-06
alma_search("1997-08-05", "1998-12-01", rrule)
#> [1] "1997-08-05" "1997-08-17" "1997-08-19" "1997-08-31"
```

### An example where an invalid date (i.e., February 30) is ignored

Invalid dates do not decrease the total count.

``` r

rrule <- weekly("2007-01-15") %>%
  recur_on_day_of_month(c(15, 30)) %>%
  recur_for_count(5)

alma_search("2007-01-15", "2007-12-31", rrule)
#> [1] "2007-01-15" "2007-01-30" "2007-02-15" "2007-03-15" "2007-03-30"
```
