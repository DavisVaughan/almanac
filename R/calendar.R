#' Calendars
#'
#' The following generate `schedule` objects corresponding to calendars. A
#' calendar is composed of multiple holiday schedules generated from the
#' `hldy_*()` functions.
#'
#' @param since `[Date(1)]`
#'
#'    The initial date to start the schedule from.
#'
#' @examples
#' on_us_holidays <- calendar_usa_federal()
#'
#' # Locate holidays
#' sch_seq("2000-01-01", "2001-01-01", on_us_holidays)
#'
#' # Adjust dates relative to holidays
#' on_weekends <- recur_on_weekends(weekly())
#' on_us_holidays_and_weekends <- sch_add_rrule(on_us_holidays, on_weekends)
#'
#' x <- as.Date("2019-08-30")
#' sch_step(x, 1, sch_add_rrule(on_us_holidays, on_weekends))
#'
#' @name calendars
NULL

# ------------------------------------------------------------------------------

#' @rdname calendars
#' @export
calendar_usa_federal <- function(since = "1970-01-01") {
  since <- vec_cast_date(since, "since")

  sch <- schedule()

  sch <- sch_add_schedule(sch, hldy_washington_birthday(since))
  sch <- sch_add_schedule(sch, hldy_memorial_day(since))
  sch <- sch_add_schedule(sch, hldy_labor_day(since))
  sch <- sch_add_schedule(sch, hldy_columbus_day(since))
  sch <- sch_add_schedule(sch, hldy_veterans_day(since))
  sch <- sch_add_schedule(sch, hldy_new_years_day(since))
  sch <- sch_add_schedule(sch, hldy_martin_luther_king_jr_day(since))
  sch <- sch_add_schedule(sch, hldy_independence_day(since))
  sch <- sch_add_schedule(sch, hldy_thanksgiving(since))
  sch <- sch_add_schedule(sch, hldy_christmas(since))

  sch
}
