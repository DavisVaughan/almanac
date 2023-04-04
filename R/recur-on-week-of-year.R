#' Recur on a week of the year
#'
#' `recur_on_week_of_year()` recurs on a specific week of the year.
#'
#' @details
#' Weekly rules are implemented according to the ISO-8601 standard. This
#' requires that the first week of a year is the first one containing at least
#' 4 days of the new year. Additionally, the week will start on the week day
#' specified by [recur_with_week_start()], which defaults to Monday.
#'
#' @param x `[rrule]`
#'
#'    A recurrence rule.
#'
#' @param week `[integer]`
#'
#'    Weeks of the year to recur on. Integer values must be between
#'    `[1, 53]` or `[-53, -1]`.
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
recur_on_week_of_year <- function(x, week) {
  validate_rrule(x, "x")

  week <- vec_cast(week, to = integer())

  abs_week <- abs(week)
  if (any(abs_week > 53 | abs_week < 1)) {
    abort("`week` can only take values in [-53, -1] and [1, 53].")
  }

  old <- get_rule(x, "week_of_year")
  if (!is_null(old)) {
    week <- vec_set_union(old, week)
  }

  week <- vec_unique(week)
  week <- vec_sort(week)

  tweak_rrule(x, week_of_year = week)
}
