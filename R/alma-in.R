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
#' @examples
#' rrule <- weekly() %>%
#'   recur_on_wday("Thursday")
#'
#' # A Thursday and Friday
#' x <- as.Date("1970-01-01") + 0:1
#'
#' alma_in(x, rrule)
#'
#' # Every month, on the 2nd day of the month
#' rrule2 <- monthly() %>%
#'   recur_on_mday(2)
#'
#' # Make a larger schedule made of multiple rules
#' sch <- schedule() %>%
#'  sch_rrule(rrule) %>%
#'  sch_rrule(rrule2)
#'
#' alma_in(x, sch)
alma_in <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  events <- schedule$cache$get()
  vec_in(x, events)
}
