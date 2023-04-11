# almanac (development version)

* New holiday and calendar API (#96):

  * `rholiday()` creates a new holiday from a holiday name and a
    rschedule that defines when the holiday occurs. There are a number of
    pre-created holidays prefixed with `hol_*()`, such as `hol_christmas()`
    and `hol_us_thanksgiving()`. Holidays are rschedules, so you can use all
    of the `alma_*()` functions on them.
    
  * `hol_observe()`, `hol_offset()`, and `hol_rename()` are three helpers for
    the holiday API. In particular, `hol_observe()` tweaks a holiday's
    _observance date_ to align with when your business actually celebrated that
    holiday.
    
  * `rcalendar()` bundles multiple holidays together into a calendar. Calendars
    are similar to `runion()`s, so you can use all the `alma_*()` functions on
    these, but they also come with their own specialized API of functions that
    start with `cal_*()`, such as `cal_match()` to look up the holiday name a
    date corresponds to, and `cal_events()` to filter for all of the holidays
    within a particular year.
    
  * `cal_us_federal()` is an example calendar representing the federal holidays
    recognized in the United States.

* `recur_for_count()` no longer overrides `until` (#95).

* New `almanac_since()` and `almanac_until()` helpers to access the default
  `since` and `until` values used for all rules (#95).

* `alma_events()` has gained a `year` argument to limit the returned set of
  events to specific years.

* New `roffset()` for creating an rschedule with events that are offset from an
  existing rschedule (#94).
  
* The `offset` argument of `recur_on_easter()` is deprecated in favor of using
  `roffset()` (#94).

* All almanac class names are now prefixed with `almanac_*` to avoid potential
  clashes with other packages.

* The `recur_with_week_start()` argument `wday` has been renamed to `day`.

* The following developer facing functions have been removed because they are
  either no longer applicable or provided extension mechanisms that are not
  very useful in practice (#93):
  
  * `new_rbundle()`
  * `new_runion()`
  * `new_rintersect()`
  * `new_rsetdiff()`
  * `rbundle_restore()`

* The family of `add_*()` functions has been deprecated (#92).

  * `add_rschedule()` has been deprecated in favor of using the `...` argument
    of `runion()`, `rintersect()`, and `rsetdiff()` directly.
    
  * `add_rdates()` has been deprecated in favor of using a combination of
    `runion()` and `rcustom()`.
    
  * `add_exdates()` has been deprecated in favor of using a combination of
    `rsetdiff()` and `rcustom()`.
    
  These functions are being aggressively deprecated and will be removed in the
  next minor version of almanac.

* `runion()`, `rintersect()`, and `rsetdiff()` have all gained `...` which
  allows you to provide the relevant rschedules on creation of these rschedules.
  This is now the preferred way to create these set-based rschedules (#91).

* New `rcustom()` for creating an rschedule from manually defined event dates
  (#90).

* The following functions have been deprecated in favor of more intuitively
  named alternatives (#83):
  
  * `recur_on_mday()` -> `recur_on_day_of_month()`
  * `recur_on_wday()` -> `recur_on_day_of_week()`
  * `recur_on_yday()` -> `recur_on_day_of_year()`
  * `recur_on_yweek()` -> `recur_on_week_of_year()`
  * `recur_on_ymonth()` -> `recur_on_month_of_year()`
  
  These functions are being aggressively deprecated and will be removed in the
  next minor version of almanac.

* Greatly improved the print methods of all almanac classes using cli (#86).

* Updated internal JavaScript rrule library to version 2.7.2 (#82).

* almanac steppers created with `stepper()` now work as `.before` and
  `.after` arguments of `slider::slide_index()` and friends (#80).

* R >=3.5.0 is now required, which is in line with tidyverse standards.

# almanac 0.1.1

* Explicitly imports `R6::R6Class()` and `V8::v8()` to avoid R CMD Check
  false alarms (#74).

* Fix USBAN error of casting `NA_real_` and `NaN` to integer (#72).

# almanac 0.1.0

* Added a `NEWS.md` file to track changes to the package.
