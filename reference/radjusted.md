# Create an adjusted rschedule

`radjusted()` creates a new adjusted rschedule on top of an existing
one. The new rschedule contains the same event dates as the existing
rschedule, except when they intersect with the dates in the event set of
the rschedule, `adjust_on`. In those cases, an `adjustment` is applied
to the problematic dates to shift them to valid event dates.

This is most useful when creating corporate holiday rschedules. For
example, Christmas always falls on December 25th, but if it falls on a
Saturday, your company might observe Christmas on the previous Friday.
If it falls on a Sunday, you might observe it on the following Monday.
In this case, you could construct an rschedule for a recurring event of
December 25th, and a second rschedule for weekends. When Christmas falls
on a weekend, you would apply an adjustment of
[`adj_nearest()`](adjustments.md) to get the observance date.

## Usage

``` r
radjusted(rschedule, adjust_on, adjustment)
```

## Arguments

- rschedule:

  `[rschedule]`

  An rschedule, such as an rrule, runion, rintersect, or rsetdiff.

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

## Value

An adjusted rschedule.

## Examples

``` r
since <- "2000-01-01"
until <- "2010-01-01"

on_christmas <- yearly(since = since, until = until) %>%
  recur_on_month_of_year("Dec") %>%
  recur_on_day_of_month(25)

# All Christmas dates, with no adjustments
alma_events(on_christmas)
#>  [1] "2000-12-25" "2001-12-25" "2002-12-25" "2003-12-25" "2004-12-25"
#>  [6] "2005-12-25" "2006-12-25" "2007-12-25" "2008-12-25" "2009-12-25"

on_weekends <- weekly(since = since, until = until) %>%
  recur_on_weekends()

# Now all Christmas dates that fell on a weekend are
# adjusted either forwards or backwards, depending on which
# non-event date was closer
on_adj_christmas <- radjusted(on_christmas, on_weekends, adj_nearest)

alma_events(on_adj_christmas)
#>  [1] "2000-12-25" "2001-12-25" "2002-12-25" "2003-12-25" "2004-12-24"
#>  [6] "2005-12-26" "2006-12-25" "2007-12-25" "2008-12-25" "2009-12-25"
```
