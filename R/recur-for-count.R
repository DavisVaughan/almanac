#' Control the number of times to recur
#'
#' @description
#' `recur_for_count()` controls the total number of events in the recurrence
#' set.
#'
#' @details
#' Remember that the number of times the occurrence has occurred is counted
#' from the `since` date and is limited by the `until` date! Adjust them as
#' necessary to get your desired results.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param n `[positive integer(1)]`
#'
#'    The number of times to recur for.
#'
#' @return
#' An updated rrule.
#'
#' @export
#' @examples
#' # Using the default `since` date
#' daily_since_epoch_for_5 <- daily() %>% recur_for_count(5)
#'
#' alma_search("1969-12-31", "1970-01-25", daily_since_epoch_for_5)
#'
#' # Changing the `since` date
#' daily_since_2019_for_5 <- daily(since = "2019-01-01") %>% recur_for_count(5)
#'
#' alma_search("2018-12-31", "2019-01-25", daily_since_2019_for_5)
#'
#' # In the case of "impossible" dates, such as 2019-02-31 and 2019-04-31 in the
#' # example below, they are not added to the total count. Only true event
#' # dates are counted.
#' on_31_for_5 <- monthly(since = "2019-01-01") %>%
#'   recur_on_day_of_month(31) %>%
#'   recur_for_count(5)
#'
#' alma_search("2019-01-01", "2020-01-01", on_31_for_5)
recur_for_count <- function(x, n) {
  check_rrule(x)
  check_rule_not_set(x, "count")

  check_number_whole(n, min = 1)
  n <- vec_cast(n, to = integer())

  tweak_rrule(x, count = n)
}
