#' Is `x` in the event set?
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
#' # Make a larger rbundle made of multiple rules
#' rb <- rbundle() %>%
#'  add_rschedule(rrule) %>%
#'  add_rschedule(rrule2)
#'
#' alma_in(x, rb)
alma_in <- function(x, rschedule) {
  x <- vec_cast_date(x)
  validate_rschedule(rschedule, "rschedule")
  events <- rschedule_events(rschedule)
  vec_in(x, events)
}
