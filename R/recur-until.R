#' Control the final date of the recurrence
#'
#' @description
#'
#' `recur_until()` controls the final date of the recurrence set. If not set,
#' the recurrence will continue indefinitely. `recur_until()`
#' is mutually exclusive with [recur_for_count()].
#'
#' @details
#'
#' Remember that the `until` date must be after the `since` date! Adjust it as
#' necessary to get your desired results.
#'
#' The `until` date is only included as an event if it passes all of the checks
#' set by the recurrence rule. Otherwise, it is just used as a boundary.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param until `[Date(1)]`
#'
#'    The final date boundary of the recurrence.
#'
#' @examples
#' # Using the default `since` date
#' daily_since_epoch_limited <- daily() %>% recur_until("1970-01-05")
#'
#' alma_seq("1969-12-31", "1970-01-25", daily_since_epoch_limited)
#'
#' # Changing the `since` date
#' daily_since_2019_limited <- daily(since = "2019-01-01") %>%
#'   recur_until("2019-01-05")
#'
#' alma_seq("2018-12-31", "2019-01-25", daily_since_2019_limited)
#'
#' @export
recur_until <- function(x, until) {
  validate_rrule(x)

  if (is_already_set(x, "until")) {
    abort("`until` has already been set for this rrule.")
  }

  if (is_already_set(x, "count")) {
    abort("`until` is mututally exclusive with `count`, and `count` has already been set.")
  }

  until <- vec_cast_date(until, "until")
  vec_assert(until, size = 1L)

  if (is.na(until)) {
    abort("`until` cannot be `NA`.")
  }

  if (until < x$rules$since) {
    abort("`until` must be greater than `since`.")
  }

  tweak_rrule(x, until = until)
}
