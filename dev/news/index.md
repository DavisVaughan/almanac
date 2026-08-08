# Changelog

## almanac (development version)

- Fixed a zero length vector issue with `std::copy()`
  ([\#107](https://github.com/DavisVaughan/almanac/issues/107)).

- R \>=4.0.0 is now required, in line with tidyverse standards.

## almanac 1.0.0

CRAN release: 2023-04-13

### New features

- New holiday and calendar API and corresponding vignette
  [`vignette("holidays-calendars")`](../articles/holidays-calendars.md)
  ([\#96](https://github.com/DavisVaughan/almanac/issues/96)):

  - [`rholiday()`](../reference/rholiday.md) creates a new holiday from
    a holiday name and a rschedule that defines when the holiday occurs.
    There are a number of pre-created holidays prefixed with `hol_*()`,
    such as [`hol_christmas()`](../reference/holidays.md) and
    [`hol_us_thanksgiving()`](../reference/holidays.md). Holidays are
    rschedules, so you can use all of the `alma_*()` functions on them.

  - [`hol_observe()`](../reference/holiday-utilities.md),
    [`hol_offset()`](../reference/holiday-utilities.md), and
    [`hol_rename()`](../reference/holiday-utilities.md) are three
    helpers for the holiday API. In particular,
    [`hol_observe()`](../reference/holiday-utilities.md) tweaks a
    holiday’s *observance date* to align with when your business
    actually celebrated that holiday.

  - [`rcalendar()`](../reference/rcalendar.md) bundles multiple holidays
    together into a calendar. Calendars are similar to
    [`runion()`](../reference/rset.md)s, so you can use all the
    `alma_*()` functions on these, but they also come with their own
    specialized API of functions that start with `cal_*()`, such as
    [`cal_match()`](../reference/cal_match.md) to look up the holiday
    name a date corresponds to, and
    [`cal_events()`](../reference/cal_events.md) to filter for all of
    the holidays within a particular year.

  - [`cal_us_federal()`](../reference/cal_us_federal.md) is an example
    calendar representing the federal holidays recognized in the United
    States.

- New [`roffset()`](../reference/roffset.md) for creating an rschedule
  with events that are offset from an existing rschedule
  ([\#94](https://github.com/DavisVaughan/almanac/issues/94)).

- New [`rcustom()`](../reference/rcustom.md) for creating an rschedule
  from manually defined event dates
  ([\#90](https://github.com/DavisVaughan/almanac/issues/90)).

- [`alma_events()`](../reference/alma_events.md) has gained a `year`
  argument to limit the returned set of events to specific years.

- [`runion()`](../reference/rset.md),
  [`rintersect()`](../reference/rset.md), and
  [`rsetdiff()`](../reference/rset.md) have all gained `...` which
  allows you to provide rschedules at creation time. This is now the
  preferred way to create these set-based rschedules
  ([\#91](https://github.com/DavisVaughan/almanac/issues/91)).

- Steppers created with [`stepper()`](../reference/stepper.md) now work
  as `.before` and `.after` arguments of
  [`slider::slide_index()`](https://slider.r-lib.org/reference/slide_index.html)
  and friends
  ([\#80](https://github.com/DavisVaughan/almanac/issues/80)).

### Lifecycle changes

- The following functions have been deprecated in favor of more
  intuitively named alternatives
  ([\#83](https://github.com/DavisVaughan/almanac/issues/83)):

  - [`recur_on_mday()`](../reference/deprecated-recur.md) -\>
    [`recur_on_day_of_month()`](../reference/recur_on_day_of_month.md)
  - [`recur_on_wday()`](../reference/deprecated-recur.md) -\>
    [`recur_on_day_of_week()`](../reference/recur_on_day_of_week.md)
  - [`recur_on_yday()`](../reference/deprecated-recur.md) -\>
    [`recur_on_day_of_year()`](../reference/recur_on_day_of_year.md)
  - [`recur_on_yweek()`](../reference/deprecated-recur.md) -\>
    [`recur_on_week_of_year()`](../reference/recur_on_week_of_year.md)
  - [`recur_on_ymonth()`](../reference/deprecated-recur.md) -\>
    [`recur_on_month_of_year()`](../reference/recur_on_month_of_year.md)

  These functions are being aggressively deprecated and will be removed
  in the next minor version of almanac.

- The family of `add_*()` functions has been deprecated
  ([\#92](https://github.com/DavisVaughan/almanac/issues/92)).

  - [`add_rschedule()`](../reference/rset-add.md) has been deprecated in
    favor of using the `...` argument of
    [`runion()`](../reference/rset.md),
    [`rintersect()`](../reference/rset.md), and
    [`rsetdiff()`](../reference/rset.md) directly.

  - [`add_rdates()`](../reference/rset-add.md) has been deprecated in
    favor of using a combination of [`runion()`](../reference/rset.md)
    and [`rcustom()`](../reference/rcustom.md).

  - [`add_exdates()`](../reference/rset-add.md) has been deprecated in
    favor of using a combination of [`rsetdiff()`](../reference/rset.md)
    and [`rcustom()`](../reference/rcustom.md).

  These functions are being aggressively deprecated and will be removed
  in the next minor version of almanac.

- The `offset` argument of
  [`recur_on_easter()`](../reference/recur_on_easter.md) is deprecated
  in favor of using [`roffset()`](../reference/roffset.md)
  ([\#94](https://github.com/DavisVaughan/almanac/issues/94)).

### Breaking changes

- All almanac class names are now prefixed with `almanac_*` to avoid
  potential clashes with other packages.

- The [`recur_with_week_start()`](../reference/recur_with_week_start.md)
  argument `wday` has been renamed to `day`.

- The following developer facing functions have been removed because
  they are either no longer applicable or provided extension mechanisms
  that are not very useful in practice
  ([\#93](https://github.com/DavisVaughan/almanac/issues/93)):

  - `new_rbundle()`
  - `new_runion()`
  - `new_rintersect()`
  - `new_rsetdiff()`
  - `rbundle_restore()`

### Bug fixes and minor improvements

- [`recur_for_count()`](../reference/recur_for_count.md) no longer
  overrides `until`
  ([\#95](https://github.com/DavisVaughan/almanac/issues/95)).

- New [`almanac_since()`](../reference/almanac-defaults.md) and
  [`almanac_until()`](../reference/almanac-defaults.md) helpers to
  access the default `since` and `until` values used for all rules
  ([\#95](https://github.com/DavisVaughan/almanac/issues/95)).

- Greatly improved the print methods of all almanac classes using cli
  ([\#86](https://github.com/DavisVaughan/almanac/issues/86)).

- Updated internal JavaScript rrule library to version 2.7.2
  ([\#82](https://github.com/DavisVaughan/almanac/issues/82)).

- R \>=3.5.0 is now required, which is in line with tidyverse standards.

## almanac 0.1.1

CRAN release: 2020-05-28

- Explicitly imports
  [`R6::R6Class()`](https://r6.r-lib.org/reference/R6Class.html) and
  [`V8::v8()`](https://jeroen.r-universe.dev/V8/reference/V8.html) to
  avoid R CMD Check false alarms
  ([\#74](https://github.com/DavisVaughan/almanac/issues/74)).

- Fix USBAN error of casting `NA_real_` and `NaN` to integer
  ([\#72](https://github.com/DavisVaughan/almanac/issues/72)).

## almanac 0.1.0

CRAN release: 2020-05-27

- Added a `NEWS.md` file to track changes to the package.
