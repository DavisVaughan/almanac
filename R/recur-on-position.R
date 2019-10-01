#' Recur on a position within a frequency
#'
#' `recur_on_position()` let's you have fine tuned control over which element
#' of the set to select _within_ the base frequency.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param n `[integer]`
#'
#'    The positions to select within an intrafrequency set. Negative numbers
#'    select from the end of the set.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#' library(magrittr)
#'
#' start <- "1999-01-01"
#' end <- "1999-05-01"
#'
#' # You might want the last day of the month that is either a
#' # Sunday or a Monday, but you don't want to return both.
#' # This would return both:
#' on_last_monday_and_sunday <- monthly() %>%
#'   recur_on_wday(c("Monday", "Sunday"), -1)
#'
#' sch_seq(start, end, on_last_monday_and_sunday)
#'
#' # To return just the last one, you would select the last value in
#' # the set, which is computed on a per month basis
#' on_very_last_monday_or_sunday <- on_last_monday_and_sunday %>%
#'   recur_on_position(-1)
#'
#' sch_seq(start, end, on_very_last_monday_or_sunday)
#'
#' wday(sch_seq(start, end, on_very_last_monday_or_sunday), label = TRUE)
#'
#' @export
recur_on_position <- function(x, n) {
  validate_rrule(x)

  if (is_already_set(x, "position")) {
    abort("`position` has already been set for this rrule.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")

  # TODO - Limit range of `n`? 366 max?

  tweak_rrule(x, position = n)
}