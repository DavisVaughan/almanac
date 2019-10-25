#' Adjust a vector of dates
#'
#' `alma_adjust()` takes an existing vector of dates and shifts it by applying
#' an `adjustment` whenever a date in `x` is also an event defined by
#' the `schedule`.
#'
#' @details
#'
#' Internally, a period / integer `adjustment` is applied repeatedly until the
#' next non-event date is found. Be careful! This can result in infinite loops
#' with improperly defined schedules, which are impossible for us to guard
#' against for you.
#'
#' A custom `adjustment` function should expect to accept the dates
#' requiring adjustment, and should _completely_ adjust them to the next
#' non-event date. It is the responsibility of the `adjustment` function to
#' ensure that the date resulting from the adjustment is not also an
#' event date.
#'
#' @param x `[Date]`
#'
#'   A vector of dates.
#'
#' @param schedule `[schedule / event]`
#'
#'   A schedule or event.
#'
#' @param adjustment `[Period(1) / integer(1) / function / formula]`
#'
#'   An adjustment to make whenever a date falls on an event.
#'
#'   If this is a lubridate period object, such as [lubridate::days()],
#'   or an integer, then the adjustment is repeatedly applied as
#'   `x + adjustment` until the next non-event date is found.
#'
#'   If this is a function or formula (i.e., a lambda function), then it
#'   should accept 2 arguments, the dates to adjust and the original `schedule`,
#'   and should return a `Date` vector of the same size as the original input
#'   containing the adjusted dates. See the functions on the help page for
#'   [adj_following()] for some examples.
#'
#' @examples
#' library(lubridate, warn.conflicts = FALSE)
#'
#' # The first of the month is an "event" so we have to adjust
#' # our current date to avoid that.
#' on_first_of_month <- monthly() %>% recur_on_mday(1)
#' alma_adjust("2019-01-01", on_first_of_month)
#'
#' # The adjustment could also be backwards
#' alma_adjust("2019-01-01", on_first_of_month, adjustment = -days(1))
#'
#' # Period adjustments are applied repeatedly until the next non-event can
#' # be found. Here, 2019-01-01 is an event, so we move to 2019-01-02, but
#' # that is an event too, so we move to 2019-01-03.
#' on_first_or_second_of_month <- monthly() %>% recur_on_mday(1:2)
#' alma_adjust("2019-01-01", on_first_or_second_of_month)
#'
#' # ---------------------------------------------------------------------------
#' # Custom adjustments
#'
#' # Financial business logic might require special rules, a few of which are
#' # encoded in the `adj_*()` functions. For example, `adj_modified_following()`
#' # will use an adjustment of `+days(1)`, unless making that adjustment would
#' # place you past the last day in the month, in which case an adjustment of
#' # `-days(1)` is made instead.
#' on_31st <- monthly() %>% recur_on_mday(31)
#' alma_adjust("2019-01-31", on_31st, adj_modified_following)
#'
#' # `adj_nearest()` looks to the closest non-event date. Here, the 13th
#' # is closer than the 18th, so it is chosen as the adjustment date.
#' on_offset_middle_of_month <- monthly() %>% recur_on_mday(14:17)
#' alma_adjust("2019-01-15", on_offset_middle_of_month, adj_nearest)
#'
#' # When the distance is the same, the following date is chosen
#' on_balanced_middle_of_month <- monthly() %>% recur_on_mday(14:16)
#' alma_adjust("2019-01-15", on_balanced_middle_of_month, adj_nearest)
#'
#' @export
alma_adjust <- function(x, schedule, adjustment = days(1)) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)
  adjuster <- make_adjuster(adjustment)

  alma_adjust_impl(x, schedule, adjuster)
}

alma_adjust_impl <- function(x, schedule, adjuster) {
  # Find initial set of events
  problem_loc <- sch_in(x, schedule)

  if (any(problem_loc)) {
    x[problem_loc] <- adjuster(x[problem_loc], schedule)
  }

  x
}

# ------------------------------------------------------------------------------

make_adjuster <- function(adjustment) {
  # Already a function?
  if (is_function(adjustment)) {
    if (length(fn_fmls_names(adjustment)) != 2L) {
      abort("An `adjustment` function must have 2 arguments.")
    }
    return(adjustment)
  }

  # character -> period
  if (is.character(adjustment)) {
    adjustment <- period(adjustment)
  }

  # period -> function
  if (is.period(adjustment)) {
    vec_assert(adjustment, size = 1L)

    if (is_subdaily(adjustment)) {
      abort("`adjustment` must not contain any sub-daily components.")
    }

    adjuster <- adj_period_factory(adjustment)
    return(adjuster)
  }

  # integer -> function
  if (is.integer(adjustment) || is.double(adjustment)) {
    adjustment <- vec_cast(adjustment, integer())
    vec_assert(adjustment, size = 1L)

    adjuster <- adj_period_factory(adjustment)
    return(adjuster)
  }

  # formula -> function
  if (is_formula(adjustment, scoped = TRUE, lhs = FALSE)) {
    adjuster <- as_function(adjustment)
    return(adjuster)
  }

  abort("`adjustment` must be a period or a function.")
}

is_subdaily <- function(x) {
  sum(abs(hour(x)), abs(minute(x)), abs(second(x))) != 0L
}
