#' Is `x` in the schedule?
#'
#' `alma_in()` checks if `x` is in the set of dates defined by the schedule.
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
alma_in <- function(x, schedule) {
  x <- vec_cast_date(x)

  schedule <- as_schedule(schedule)

  min <- min2(x)
  max <- max2(x)

  events <- alma_seq_impl(min, max, schedule, inclusive = TRUE)

  vec_in(x, events)
}

min2 <- function(x) {
  suppressWarnings(min(x, na.rm = TRUE))
}

max2 <- function(x) {
  suppressWarnings(max(x, na.rm = TRUE))
}
