#' Recur on a day of the year
#'
#' `recur_on_yday()` recurs on a specific day of the year.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param yday `[integer]`
#'
#'    Days of the year to recur on. Values must be from `[-366, -1]` and
#'    `[1, 366]`.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' on_5th_day_of_year <- yearly() %>% recur_on_yday(5)
#'
#' alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year)
#'
#' # Notice that if you use a `since` date that has a day of the year
#' # after the specified one, it rolls to the next year
#' on_5th_day_of_year2 <- yearly(since = "1999-01-06") %>% recur_on_yday(5)
#' alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year2)
#'
#' # Negative values select from the back, which is useful in leap years
#' leap_year(as.Date("2000-01-01"))
#'
#' last_day_of_year <- yearly() %>% recur_on_yday(-1)
#' last_day_of_year_bad <- yearly() %>% recur_on_yday(365)
#'
#' alma_search("1999-01-01", "2000-12-31", last_day_of_year)
#' alma_search("1999-01-01", "2000-12-31", last_day_of_year_bad)
#'
#' @export
recur_on_yday <- function(x, yday) {
  validate_rrule(x)

  old <- get_rule(x, "yday")
  new <- vec_cast(yday, integer(), x_arg = "yday")

  abs_new <- abs(new)
  if (any(abs_new > 366 | abs_new < 1)) {
    abort("`yday` can only take values in [-366, -1] and [1, 366].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, yday = new)
}
