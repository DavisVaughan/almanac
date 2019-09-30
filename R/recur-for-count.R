#' Control the number of times to recur
#'
#' @description
#'
#' `recur_for_count()` controls the number of times that a recurrence will recur
#' for. If not set, the recurrence will continue indefinitely.
#' `recur_for_count()` is mutually exclusive with [recur_until()].
#'
#' @details
#'
#' Remember that the number of times the occurrance has occurred is counted
#' from the `since` date! Adjust it as necessary to get your desired results.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param n `[positive integer(1)]`
#'
#'    The number of times to recur for.
#'
#' @examples
#' library(magrittr)
#'
#' # Using the default `since` date
#' daily_since_epoch_for_5 <- daily() %>% recur_for_count(5)
#'
#' sch_seq("1969-12-31", "1970-01-25", daily_since_epoch_for_5)
#'
#' # Changing the `since` date
#' daily_since_2019_for_5 <- daily(since = "2019-01-01") %>% recur_for_count(5)
#'
#' sch_seq("2018-12-31", "2019-01-25", daily_since_2019_for_5)
#'
#' # In the case of "impossible" dates, such as 2019-02-31 and 2019-04-31 in the
#' # example below, they are not added to the total count. Only true event
#' # dates are counted.
#' on_31_for_5 <- monthly(since = "2019-01-01") %>%
#'   recur_on_mday(31) %>%
#'   recur_for_count(5)
#'
#' sch_seq("2019-01-01", "2020-01-01", on_31_for_5)
#'
#' @export
recur_for_count <- function(x, n) {
  validate_rrule(x)

  if (is_already_set(x, "count")) {
    abort("`count` has already been set for this rrule.")
  }

  if (is_already_set(x, "until")) {
    abort("`count` is mututally exclusive with `until`, and `until` has already been set.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")
  vec_assert(n, size = 1L)

  if (n <= 0L) {
    abort("`n` must be greater than 0.")
  }

  tweak_rrule(x, count = n)
}
