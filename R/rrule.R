#' Create a recurrence rule
#'
#' @description
#' These functions allow you to create a recurrence rule with a specified
#' frequency. They are the base elements for all recurrence rules. To add
#' to them, use one of the `recur_*()` functions.
#'
#' - `daily()` Recur on a daily frequency.
#'
#' - `weekly()` Recur on a weekly frequency.
#'
#' - `monthly()` Recur on a monthly frequency.
#'
#' - `yearly()` Recur on a yearly frequency.
#'
#' @details
#' By default, `since == "1900-01-01"` and `until == "2100-01-01"`, which should
#' capture most use cases well while still being performant. You may need to
#' adjust these dates if you want events outside this range.
#'
#' In terms of speed, it is generally more efficient if you adjust the `since`
#' and `until` date to be closer to the first date in the sequence of dates
#' that you are working with. For example, if you are working with dates in the
#' range of 2019 and forward, adjust the `since` date to be `2019-01-01` for a
#' significant speed boost.
#'
#' As the anchor date, events are often calculated _relative to_ this
#' date. As an example, a rule of "on Monday, every other week" would use
#' the `since` date to find the first Monday to start the recurrence from.
#'
#' There is no `quarterly()` recurrence frequency, but this can be accomplished
#' with `monthly() %>% recur_on_interval(3)`. The month to start the quarterly
#' interval from will be pulled from the `since` date inside `monthly()`. The
#' default will use a quarterly rule starting in January since the default
#' `since` date is `1900-01-01`. See the examples.
#'
#' @param since `[Date(1)]`
#'
#'    The lower bound on the event set. Depending on the final
#'    recurrence rule, pieces of information from this anchor date might be used
#'    to generate a complete recurrence rule.
#'
#' @param until `[Date(1)]`
#'
#'    The upper bound on the event set.
#'
#' @return
#' A new empty rrule.
#'
#' @examples
#' rrule <- monthly() %>% recur_on_day_of_month(25)
#'
#' alma_search("1970-01-01", "1971-01-01", rrule)
#'
#' # Notice that dates before 1900-01-01 are never generated with the defaults!
#' alma_search("1899-01-01", "1901-01-01", rrule)
#'
#' # Adjust the `since` date to get access to these dates
#' rrule_pre_1900 <- monthly(since = "1850-01-01") %>% recur_on_day_of_month(25)
#' alma_search("1899-01-01", "1901-01-01", rrule_pre_1900)
#'
#' # A quarterly recurrence rule can be built from
#' # `monthly()` and `recur_on_interval()`
#' on_first_of_the_quarter <- monthly() %>%
#'   recur_on_interval(3) %>%
#'   recur_on_day_of_month(1)
#'
#' alma_search("1999-01-01", "2000-04-01", on_first_of_the_quarter)
#'
#' # Alter the starting quarter by altering the `since` date
#' on_first_of_the_quarter_starting_in_feb <- monthly(since = "1998-02-01") %>%
#'   recur_on_interval(3) %>%
#'   recur_on_day_of_month(1)
#'
#' alma_search(
#'   "1999-01-01",
#'   "2000-04-01",
#'   on_first_of_the_quarter_starting_in_feb
#' )
#'
#' @name rrule
NULL

#' @rdname rrule
#' @export
daily <- function(since = "1900-01-01", until = "2100-01-01") {
  rrule(since, until, frequency = "daily")
}

#' @rdname rrule
#' @export
weekly <- function(since = "1900-01-01", until = "2100-01-01") {
  rrule(since, until, frequency = "weekly")
}

#' @rdname rrule
#' @export
monthly <- function(since = "1900-01-01", until = "2100-01-01") {
  rrule(since, until, frequency = "monthly")
}

#' @rdname rrule
#' @export
yearly <- function(since = "1900-01-01", until = "2100-01-01") {
  rrule(since, until, frequency = "yearly")
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rrule <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

rrule <- function(since, until, frequency) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  new_rrule(
    since = since,
    until = until,
    frequency = frequency
  )
}

new_rrule <- function(since = as.Date("1900-01-01"),
                      until = as.Date("2100-01-01"),
                      frequency = "yearly",
                      count = NULL,
                      interval = NULL,
                      week_start = NULL,
                      ymonth = NULL,
                      yweek = NULL,
                      day_of_year = NULL,
                      day_of_month = NULL,
                      day_of_week = NULL,
                      position = NULL,
                      easter = NULL) {
  rules <- list(
    since = since,
    until = until,
    frequency = frequency,
    count = count,
    interval = interval,
    week_start = week_start,
    ymonth = ymonth,
    yweek = yweek,
    day_of_year = day_of_year,
    day_of_month = day_of_month,
    day_of_week = day_of_week,
    position = position,
    easter = easter
  )

  cache <- cache_rrule$new(rules = rules)

  new_rschedule(
    rules = rules,
    cache = cache,
    class = "rrule"
  )
}

# ------------------------------------------------------------------------------

is_rrule <- function(x) {
  inherits(x, "rrule")
}

all_are_rrules <- function(x) {
  all(map_lgl(x, is_rrule))
}

validate_rrule <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_rrule(x)) {
    glubort("Input{x_arg} must be an rrule.")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

check_since <- function(since) {
  since <- vec_cast_date(since, "since")
  vec_assert(since, size = 1L)

  if (is_missing_or_infinite(since)) {
    abort("`since` must be a finite date.")
  }

  validate_date_bounds(since, x_arg = "since")

  since
}

check_until <- function(until) {
  until <- vec_cast_date(until, "until")
  vec_assert(until, size = 1L)

  if (is_missing_or_infinite(until)) {
    abort("`until` must be a finite date.")
  }

  validate_date_bounds(until, x_arg = "until")

  until
}

# ------------------------------------------------------------------------------

tweak_rrule <- function(x, ...) {
  new <- list2(...)
  old <- x$rules
  old[names(new)] <- new
  exec(new_rrule, !!!old)
}
