#' Is `x` in the schedule?
#'
#' `sch_in()` checks if `x` is in the set of dates defined by the schedule.
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param schedule `[schedule / rrule]`
#'
#'    A schedule or rrule.
#'
#' @export
sch_in <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  min <- min(x)
  max <- max(x)

  events <- sch_seq_impl(min, max, schedule)

  vec_in(x, events)
}
