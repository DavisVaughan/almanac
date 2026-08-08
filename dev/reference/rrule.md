# Create a recurrence rule

These functions allow you to create a recurrence rule with a specified
frequency. They are the base elements for all recurrence rules. To add
to them, use one of the `recur_*()` functions.

- `daily()` Recur on a daily frequency.

- `weekly()` Recur on a weekly frequency.

- `monthly()` Recur on a monthly frequency.

- `yearly()` Recur on a yearly frequency.

## Usage

``` r
daily(since = NULL, until = NULL)

weekly(since = NULL, until = NULL)

monthly(since = NULL, until = NULL)

yearly(since = NULL, until = NULL)
```

## Arguments

- since:

  `[Date(1)]`

  The lower bound on the event set. Depending on the final recurrence
  rule, pieces of information from this anchor date might be used to
  generate a complete recurrence rule.

- until:

  `[Date(1)]`

  The upper bound on the event set.

## Value

A new empty rrule.

## Details

By default, `since == "1900-01-01"` and `until == "2100-01-01"`, which
should capture most use cases well while still being performant. You may
need to adjust these dates if you want events outside this range. See
[`almanac_since()`](almanac-defaults.md) and
[`almanac_until()`](almanac-defaults.md) for more information.

In terms of speed, it is generally more efficient if you adjust the
`since` and `until` date to be closer to the first date in the sequence
of dates that you are working with. For example, if you are working with
dates in the range of 2019 and forward, adjust the `since` date to be
`2019-01-01` for a significant speed boost.

As the anchor date, events are often calculated *relative to* this date.
As an example, a rule of "on Monday, every other week" would use the
`since` date to find the first Monday to start the recurrence from.

There is no `quarterly()` recurrence frequency, but this can be
accomplished with `monthly() %>% recur_on_interval(3)`. The month to
start the quarterly interval from will be pulled from the `since` date
inside `monthly()`. The default will use a quarterly rule starting in
January since the default `since` date is `1900-01-01`. See the
examples.

## Examples

``` r
rrule <- monthly() %>% recur_on_day_of_month(25)

alma_search("1970-01-01", "1971-01-01", rrule)
#>  [1] "1970-01-25" "1970-02-25" "1970-03-25" "1970-04-25" "1970-05-25"
#>  [6] "1970-06-25" "1970-07-25" "1970-08-25" "1970-09-25" "1970-10-25"
#> [11] "1970-11-25" "1970-12-25"

# Notice that dates before 1900-01-01 are never generated with the defaults!
alma_search("1899-01-01", "1901-01-01", rrule)
#>  [1] "1900-01-25" "1900-02-25" "1900-03-25" "1900-04-25" "1900-05-25"
#>  [6] "1900-06-25" "1900-07-25" "1900-08-25" "1900-09-25" "1900-10-25"
#> [11] "1900-11-25" "1900-12-25"

# Adjust the `since` date to get access to these dates
rrule_pre_1900 <- monthly(since = "1850-01-01") %>% recur_on_day_of_month(25)
alma_search("1899-01-01", "1901-01-01", rrule_pre_1900)
#>  [1] "1899-01-25" "1899-02-25" "1899-03-25" "1899-04-25" "1899-05-25"
#>  [6] "1899-06-25" "1899-07-25" "1899-08-25" "1899-09-25" "1899-10-25"
#> [11] "1899-11-25" "1899-12-25" "1900-01-25" "1900-02-25" "1900-03-25"
#> [16] "1900-04-25" "1900-05-25" "1900-06-25" "1900-07-25" "1900-08-25"
#> [21] "1900-09-25" "1900-10-25" "1900-11-25" "1900-12-25"

# A quarterly recurrence rule can be built from
# `monthly()` and `recur_on_interval()`
on_first_of_the_quarter <- monthly() %>%
  recur_on_interval(3) %>%
  recur_on_day_of_month(1)

alma_search("1999-01-01", "2000-04-01", on_first_of_the_quarter)
#> [1] "1999-01-01" "1999-04-01" "1999-07-01" "1999-10-01" "2000-01-01"
#> [6] "2000-04-01"

# Alter the starting quarter by altering the `since` date
on_first_of_the_quarter_starting_in_feb <- monthly(since = "1998-02-01") %>%
  recur_on_interval(3) %>%
  recur_on_day_of_month(1)

alma_search(
  "1999-01-01",
  "2000-04-01",
  on_first_of_the_quarter_starting_in_feb
)
#> [1] "1999-02-01" "1999-05-01" "1999-08-01" "1999-11-01" "2000-02-01"
```
