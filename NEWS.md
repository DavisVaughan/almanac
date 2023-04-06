# almanac (development version)

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
