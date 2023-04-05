#' Recur on a day of the year
#'
#' `recur_on_day_of_year()` recurs on a specific day of the year.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param day `[integer]`
#'
#'    Days of the year to recur on. Values must be from `[-366, -1]` and
#'    `[1, 366]`.
#'
#' @return
#' An updated rrule.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' on_5th_day_of_year <- yearly() %>% recur_on_day_of_year(5)
#'
#' alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year)
#'
#' # Notice that if you use a `since` date that has a day of the year
#' # after the specified one, it rolls to the next year
#' on_5th_day_of_year2 <- yearly(since = "1999-01-06") %>% recur_on_day_of_year(5)
#' alma_search("1999-01-01", "2000-12-31", on_5th_day_of_year2)
#'
#' # Negative values select from the back, which is useful in leap years
#' leap_year(as.Date("2000-01-01"))
#'
#' last_day_of_year <- yearly() %>% recur_on_day_of_year(-1)
#' last_day_of_year_bad <- yearly() %>% recur_on_day_of_year(365)
#'
#' alma_search("1999-01-01", "2000-12-31", last_day_of_year)
#' alma_search("1999-01-01", "2000-12-31", last_day_of_year_bad)
#'
#' @export
recur_on_day_of_year <- function(x, day) {
  check_rrule(x)

  day <- vec_cast(day, to = integer())
  check_no_missing(day)

  abs_day <- abs(day)
  if (any(abs_day > 366 | abs_day < 1)) {
    abort("`day` can only take values in [-366, -1] and [1, 366].")
  }

  old <- get_rule(x, "day_of_year")
  if (!is_null(old)) {
    day <- vec_set_union(old, day)
  }

  day <- vec_unique(day)
  day <- vec_sort(day)

  tweak_rrule(x, day_of_year = day)
}
