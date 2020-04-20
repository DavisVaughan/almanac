#' Is `x` in the recurrence set?
#'
#' `alma_in()` checks if `x` is in the recurrence set of dates defined by the
#' rbundle.
#'
#' @param x `[Date]`
#'
#'    A vector of dates.
#'
#' @param rbundle `[rbundle / rrule]`
#'
#'    An rbundle or rrule.
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
#'  add_rrule(rrule) %>%
#'  add_rrule(rrule2)
#'
#' alma_in(x, rb)
alma_in <- function(x, rbundle) {
  x <- vec_cast_date(x)
  cacher <- as_cacher(rbundle)
  events <- cacher_events(cacher)
  vec_in(x, events)
}
