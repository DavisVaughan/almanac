#' Add to a schedule
#'
#' @description
#'
#' - `sch_rrule()` adds a `rrule` to a schedule.
#'
#' - `sch_rdate()` adds a `rdate` to a schedule. `rdate`s are singular
#'   special cased dates that are forcibly included in the schedule.
#'
#' - `sch_exdate()` adds a `exdate` to a schedule. `exdate`s are singular
#'   special cased dates that are forcibly excluded from the schedule.
#'
#' - `sch_merge()` merges two schedules together.
#'
#' @details
#'
#' In terms of priority:
#'
#' - An `exdate` will never be included.
#'
#' - A `rdate` will always be included if it is not also an `exdate`.
#'
#' - An event generated from a `rrule` will always be included if it is not
#'   also an `exdate`.
#'
#' Combining two `rrule`s into the same schedule is a way of joining them using
#' an "or" condition. For example, joining `monthly() %>% on_mday(5)` with
#' `weekly() %>% on_wday("Tuesday")` would translate to "the 5th day of the
#' month or any Tuesday".
#'
#' @param x `[schedule]`
#'
#'   A schedule.
#'
#' @param rrule `[rrule]`
#'
#'   An rrule to add to the schedule.
#'
#' @param rdate `[Date]`
#'
#'   Dates to forcibly include in the schedule.
#'
#' @param exdate `[Date]`
#'
#'   Dates to forcibly exclude from the schedule.
#'
#' @param schedule `[schedule]`
#'
#'   A schedule to merge with `x`.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' on_thanksgiving <- yearly() %>%
#'   recur_on_wday("Thurs", 4) %>%
#'   recur_on_ymonth("Nov")
#'
#' on_christmas <- yearly() %>%
#'   recur_on_mday(25) %>%
#'   recur_on_ymonth("Dec")
#'
#' on_labor_day <- monthly() %>%
#'   recur_on_ymonth("Sep") %>%
#'   recur_on_wday("Mon", 1)
#'
#' sch <- schedule() %>%
#'   sch_rrule(on_thanksgiving) %>%
#'   sch_rrule(on_christmas) %>%
#'   sch_rrule(on_labor_day)
#'
#' # Thanksgiving, Christmas, or Labor Day
#' alma_seq("2019-01-01", "2021-01-01", sch)
#'
#' # Except Labor Day in 2019
#' sch2 <- sch %>%
#'   sch_exdate("2019-09-02")
#'
#' alma_seq("2019-01-01", "2021-01-01", sch2)
#'
#' @name sch-add
NULL

#' @rdname sch-add
#' @export
sch_rrule <- function(x, rrule) {
  validate_schedule(x)
  validate_rrule(rrule, arg = "`rrule`")

  rrules <- c(x$rrules, list(rrule))

  new_schedule(
    rrules = rrules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

#' @rdname sch-add
#' @export
sch_rdate <- function(x, rdate) {
  validate_schedule(x)
  rdate <- vec_cast_date(rdate, "rdate")

  rdates <- vec_c(x$rdates, rdate)

  new_schedule(
    rrules = x$rrules,
    rdates = rdates,
    exdates = x$exdates
  )
}

#' @rdname sch-add
#' @export
sch_exdate <- function(x, exdate) {
  validate_schedule(x)
  exdate <- vec_cast_date(exdate, "exdate")

  exdates <- vec_c(x$exdates, exdate)

  new_schedule(
    rrules = x$rrules,
    rdates = x$rdates,
    exdates = exdates
  )
}

#' @rdname sch-add
#' @export
sch_merge <- function(x, schedule) {
  validate_schedule(x)
  validate_schedule(schedule, "`schedule`")

  new_rrules <- c(x$rrules, y$rrules)

  new_rdates <- c(x$rdates, y$rdates)
  new_rdates <- unique(new_rdates)

  new_exdates <- c(x$exdates, y$exdates)
  new_exdates <- unique(new_exdates)

  new_schedule(
    rrules = new_rrules,
    rdates = new_rdates,
    exdates = new_exdates
  )
}
