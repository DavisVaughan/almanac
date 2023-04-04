#' Recur on a day of the month
#'
#' `recur_on_day_of_month()` recurs on a specific day of the month.
#'
#' @details
#' If the day of the month doesn't exist for that particular month, then it
#' is ignored. For example, if `recur_on_day_of_month(30)` is set, then it will
#' never generate an event in February.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param day `[integer]`
#'
#'    The days of the month on which to recur. Negative values are allowed,
#'    which specify `n` days from the end of the month.
#'
#' @return
#' An updated rrule.
#'
#' @export
#' @examples
#' # When used with a yearly or monthly frequency, `recur_on_day_of_month()`
#' # expands the number of days in the event set.
#' on_yearly <- yearly()
#' on_yearly_day_of_month_1_to_2 <- on_yearly %>% recur_on_day_of_month(1:2)
#'
#' start <- "1999-01-01"
#' end <- "2000-06-30"
#'
#' alma_search(start, end, on_yearly)
#' alma_search(start, end, on_yearly_day_of_month_1_to_2)
#'
#' # When used with a daily frequency, `recur_on_day_of_month()` limits the
#' # number of days in the event set.
#' on_daily <- daily()
#' on_daily_day_of_month_1_to_2 <- on_daily %>% recur_on_day_of_month(1:2)
#'
#' length(alma_search(start, end, on_daily))
#' length(alma_search(start, end, on_daily_day_of_month_1_to_2))
#'
#' # Using a negative value is a powerful way to look back from the end of the
#' # month. This is particularly useful because months don't have the same
#' # number of days.
#' on_last_of_month <- monthly() %>% recur_on_day_of_month(-1)
#'
#' alma_search(start, end, on_last_of_month)
#'
#' # If you want particular days of the week at the end of the month, you
#' # could use something like this, which checks if the end of the month
#' # is also a Friday.
#' on_last_of_month_that_is_also_friday <- on_last_of_month %>% recur_on_day_of_week("Friday")
#' alma_search(start, end, on_last_of_month_that_is_also_friday)
#'
#' # But you probably wanted this, which takes the last friday of the month,
#' # on whatever day that lands on
#' on_last_friday_of_month <- monthly() %>% recur_on_day_of_week("Friday", nth = -1)
#' alma_search(start, end, on_last_friday_of_month)
recur_on_day_of_month <- function(x, day) {
  validate_rrule(x, "x")

  day <- vec_cast(day, to = integer())

  abs_day <- abs(day)
  if (any(abs_day > 31 | abs_day < 1)) {
    abort("`day` can only take values in [-31, -1] and [1, 31].")
  }

  old <- get_rule(x, "day_of_month")
  if (!is_null(old)) {
    day <- vec_set_union(old, day)
  }

  day <- vec_unique(day)
  day <- vec_sort(day)

  tweak_rrule(x, day_of_month = day)
}
