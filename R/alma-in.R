#' Check if dates are in an event set
#'
#' `alma_in()` checks if `x` is in the event set of dates defined by the
#' rschedule.
#'
#' @inheritParams adj_following
#'
#' @return
#' A logical vector the same size as `x`.
#'
#' @export
#' @examples
#' rrule <- weekly() %>%
#'   recur_on_day_of_week("Thursday")
#'
#' # A Thursday and Friday
#' x <- as.Date("1970-01-01") + 0:1
#'
#' alma_in(x, rrule)
#'
#' # Every month, on the 2nd day of the month
#' rrule2 <- monthly() %>%
#'   recur_on_day_of_month(2)
#'
#' # Make a larger rbundle made of multiple rules
#' rb <- runion(rrule, rrule2)
#'
#' alma_in(x, rb)
alma_in <- function(x, rschedule) {
  x <- vec_cast_date(x)
  check_rschedule(rschedule)
  events <- rschedule_events(rschedule)
  vec_in(x, events)
}
