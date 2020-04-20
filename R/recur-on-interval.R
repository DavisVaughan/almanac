#' Recur on an interval
#'
#' `recur_on_interval()` adjusts the interval of the base frequency of the
#' recurrence rule. For example, a [monthly()] rule with an interval of 2 would
#' become "every other month".
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param n `[positive integer(1)]`
#'
#'    The interval on which to recur.
#'
#' @examples
#' # The default interval is 1
#' on_monthly <- monthly(since = "1999-01-01")
#'
#' alma_search("1999-01-01", "1999-06-01", on_monthly)
#'
#' # Adjust to every other month
#' on_every_other_month <- on_monthly %>% recur_on_interval(2)
#'
#' alma_search("1999-01-01", "1999-06-01", on_every_other_month)
#'
#' # Note that the frequency is limited to "every other month", but you
#' # can still have multiple events inside a single month
#' on_every_other_month_on_mday_25_or_26 <- on_every_other_month %>%
#'   recur_on_mday(25:26)
#'
#' alma_search("1999-01-01", "1999-06-01", on_every_other_month_on_mday_25_or_26)
#'
#' @export
recur_on_interval <- function(x, n) {
  validate_rrule(x)

  if (is_already_set(x, "interval")) {
    abort("`interval` has already been set for this rrule.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")
  vec_assert(n, size = 1L)

  if (n <= 0L) {
    abort("`n` must be greater than 0.")
  }

  tweak_rrule(x, interval = n)
}
