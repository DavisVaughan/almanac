#' Recur on a day of the month
#'
#' `recur_on_mday()` recurs on a specific day of the month.
#'
#' @details
#'
#' If the day of the month doesn't exist for that particular month, then it
#' is ignored. For example, if `recur_on_mday(30)` is set, then it will never
#' generate an event in February.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param mday `[integer]`
#'
#'    The days of the month on which to recur. Negative values are allowed,
#'    which specify n days from the end of the month.
#'
#' @return
#' An updated rrule.
#'
#' @examples
#' # When used with a yearly or monthly frequency, `recur_on_mday()` expands the
#' # number of days in the recurrence set.
#' on_yearly <- yearly()
#' on_yearly_mday_1_to_2 <- on_yearly %>% recur_on_mday(1:2)
#'
#' start <- "1999-01-01"
#' end <- "2000-06-30"
#'
#' alma_search(start, end, on_yearly)
#' alma_search(start, end, on_yearly_mday_1_to_2)
#'
#' # When used with a daily frequency, `recur_on_mday()` limits the number of
#' # days in the recurrence set.
#' on_daily <- daily()
#' on_daily_mday_1_to_2 <- on_daily %>% recur_on_mday(1:2)
#'
#' length(alma_search(start, end, on_daily))
#' length(alma_search(start, end, on_daily_mday_1_to_2))
#'
#' # Using a negative value is a powerful way to look back from the end of the
#' # month. This is particularly useful because months don't have the same
#' # number of days.
#' on_last_of_month <- monthly() %>% recur_on_mday(-1)
#'
#' alma_search(start, end, on_last_of_month)
#'
#' # If you want particular days of the week at the end of the month, you
#' # could use something like this, which checks if the end of the month
#' # is also a Friday.
#' on_last_of_month_that_is_also_friday <- on_last_of_month %>% recur_on_wday("Friday")
#' alma_search(start, end, on_last_of_month_that_is_also_friday)
#'
#' # But you probably wanted this, which takes the last friday of the month,
#' # on whatever day that lands on
#' on_last_friday_of_month <- monthly() %>% recur_on_wday("Friday", -1)
#' alma_search(start, end, on_last_friday_of_month)
#'
#' @export
recur_on_mday <- function(x, mday) {
  validate_rrule(x, "x")

  old <- get_rule(x, "mday")
  new <- vec_cast(mday, integer(), x_arg = "mday")

  abs_new <- abs(new)
  if (any(abs_new > 31 | abs_new < 1)) {
    abort("`mday` can only take values in [-31, -1] and [1, 31].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, mday = new)
}
