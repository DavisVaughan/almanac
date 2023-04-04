#' Control the start of the week
#'
#' @description
#'
#' `recur_with_week_start()` controls the week day that represents the start of
#' the week. This is important for rules that use [recur_on_week_of_year()].
#'
#' _The default day of the week to start on is Monday._
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param wday `[integer(1) / character(1)]`
#'
#'    Day of the week to start the week on. Must be an integer value in
#'    `[1, 7]`, with `1 = Monday` and `7 = Sunday`. This is also allowed to be
#'    a full weekday string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @return
#' An updated rrule.
#'
#' @export
#' @examples
#' # Weekly rules are a bit tricky because they are implemented to comply
#' # with ISO-8601 standards, which require that the first week of the year
#' # is when there are at least 4 days in that year, and the week starts on
#' # the week day specified by `recur_with_week_start()` (Monday by default).
#' on_first_week <- yearly() %>% recur_on_week_of_year(1)
#'
#' # In 2017:
#' # - Look at dates 1-4
#' # - 2017-01-02 is a Monday, so start the first week here
#' alma_search("2017-01-01", "2017-01-25", on_first_week)
#'
#' # In 2015:
#' # - Look at dates 1-4
#' # - None of these are Monday, so the start of the week is
#' #   in the previous year
#' # - Look at 2014 and find the last Monday, 2014-12-29. This is the start of
#' #   the first week in 2015.
#' alma_search("2014-12-25", "2015-01-25", on_first_week)
#'
#' # Say we want the start of the week to be Sunday instead of Monday!
#'
#' # In 2015:
#' # - Look at dates 1-4
#' # - 2015-01-04 is a Sunday, so start the first week here
#' on_first_week_sun <- yearly() %>%
#'   recur_on_week_of_year(1) %>%
#'   recur_with_week_start("Sunday")
#'
#' alma_search("2014-12-25", "2015-01-25", on_first_week_sun)
recur_with_week_start <- function(x, wday) {
  validate_rrule(x, "x")

  wday <- normalize_day_of_week(wday)

  wday <- vec_cast(wday, integer(), x_arg = "wday")
  vec_assert(wday, size = 1L)

  if (wday < 1L || wday > 7L) {
    abort("`wday` must be an integer between 1 and 7.")
  }

  tweak_rrule(x, week_start = wday)
}
