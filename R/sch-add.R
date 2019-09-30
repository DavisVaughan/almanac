#' Add to a schedule
#'
#' @description
#'
#' - `sch_add_rrule()` adds a `rrule` to a schedule.
#'
#' - `sch_add_rdate()` adds a `rdate` to a schedule. `rdate`s are singular
#'   special cased dates that are forcibly included in the schedule.
#'
#' - `sch_add_exdate()` adds a `exdate` to a schedule. `exdate`s are singular
#'   special cased dates that are forcibly excluded from the schedule.
#'
#' - `sch_add_schedule()` merges two schedules together.
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
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' on_easter <- yearly() %>%
#'   rr_on_easter()
#'
#' on_christmas <- yearly() %>%
#'   rr_on_mday(25) %>%
#'   rr_on_ymonth("Dec")
#'
#' on_labor_day <- monthly() %>%
#'   rr_on_ymonth("Sep") %>%
#'   rr_on_wday("Mon", 1)
#'
#' sch <- schedule() %>%
#'   sch_add_rrule(on_easter) %>%
#'   sch_add_rrule(on_christmas) %>%
#'   sch_add_rrule(on_labor_day)
#'
#' # Christmas, Easter, or Labor Day
#' sch_seq("2019-01-01", "2021-01-01", sch)
#'
#' # Except Labor Day in 2019
#' sch2 <- sch %>%
#'   sch_add_exdate("2019-09-02")
#'
#' sch_seq("2019-01-01", "2021-01-01", sch2)
#'
#' @name sch-add
NULL

#' @rdname sch-add
#' @export
sch_add_rrule <- function(x, rrule) {
  validate_schedule(x)
  validate_rrule(rrule, arg = "`rrule`")

  recurrences <- x$recurrences
  recurrences$rrules <- c(recurrences$rrules, list(rrule))

  new_schedule(
    rrules = recurrences$rrules,
    rdates = recurrences$rdates,
    exdates = recurrences$exdates
  )
}

#' @rdname sch-add
#' @export
sch_add_rdate <- function(x, rdate) {
  validate_schedule(x)
  rdate <- vec_cast_date(rdate, "rdate")

  recurrences <- x$recurrences
  recurrences$rdates <- c(recurrences$rdates, list(rdate))

  new_schedule(
    rrules = recurrences$rrules,
    rdates = recurrences$rdates,
    exdates = recurrences$exdates
  )
}

#' @rdname sch-add
#' @export
sch_add_exdate <- function(x, exdate) {
  validate_schedule(x)
  exdate <- vec_cast_date(exdate, "exdate")

  recurrences <- x$recurrences
  recurrences$exdates <- c(recurrences$exdates, list(exdate))

  new_schedule(
    rrules = recurrences$rrules,
    rdates = recurrences$rdates,
    exdates = recurrences$exdates
  )
}

#' @rdname sch-add
#' @export
sch_add_schedule <- function(x, schedule) {
  validate_schedule(x)
  validate_schedule(schedule, "`schedule`")

  x_recurrences <- x$recurrences
  y_recurrences <- schedule$recurrences

  new_rrules <- c(x_recurrences$rrules, y_recurrences$rrules)

  new_rdates <- c(x_recurrences$rdates, y_recurrences$rdates)
  new_rdates <- unique(new_rdates)

  new_exdates <- c(x_recurrences$exdates, y_recurrences$exdates)
  new_exdates <- unique(new_exdates)

  new_schedule(
    rrules = new_rrules,
    rdates = new_rdates,
    exdates = new_exdates
  )
}
