#' Date adjustments
#'
#' @description
#' This family of `adj_*()` functions encode business logic for common
#' date adjustments. If `x` falls on an event date, it is adjusted according to
#' the function's adjustment rule. Otherwise it is left untouched.
#'
#'   - `adj_following()`
#'
#'     Choose the first non-event date after `x`.
#'
#'   - `adj_preceding()`
#'
#'     Choose the first non-event date before `x`.
#'
#'   - `adj_modified_following()`
#'
#'     Choose the first non-event date after `x`, unless it falls in a
#'     different month, in which case the first non-event date before `x` is
#'     chosen instead.
#'
#'   - `adj_modified_preceding()`
#'
#'     Choose the first non-event date before `x`, unless it falls in a
#'     different month, in which case the first non-event date after `x` is
#'     chosen instead.
#'
#'   - `adj_nearest()`
#'
#'     Choose the nearest non-event date to `x`. If the closest preceding and
#'     following non-event dates are equally far away, the following non-event
#'     date is chosen.
#'
#' @param x `[Date]`
#'
#'   A vector of dates.
#'
#' @param schedule `[schedule / rrule]`
#'
#'   A schedule or rrule.
#'
#' @name adjustments
#'
#' @examples
#' # A Saturday
#' x <- as.Date("1970-01-03")
#'
#' on_weekends <- weekly() %>% recur_on_weekends()
#'
#' # Adjust forward to Monday
#' adj_following(x, on_weekends)
#'
#' # Adjust backwards to Friday
#' adj_preceding(x, on_weekends)
#'
#' # Adjust to nearest non-event date
#' adj_nearest(x, on_weekends)
#' adj_nearest(x + 1, on_weekends)
#'
#' # Sundays, one of which is at the end of the month
#' sundays <- as.Date(c("2020-05-24", "2020-05-31"))
#'
#' # Adjust forward, unless that takes us into a new month, in which case we
#' # adjust backwards.
#' adj_modified_following(sundays, on_weekends)
#'
#' # Saturdays, one of which is at the beginning of the month
#' saturdays <- as.Date(c("2020-08-01", "2020-08-08"))
#'
#' # Adjust backwards, unless that takes us into a new month, in which
#' # case we adjust forwards
#' adj_modified_preceding(saturdays, on_weekends)
NULL

#' @rdname adjustments
#' @export
adj_following <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get_events()

  adj_following_impl(x, events)
}

adj_following_impl <- function(x, events) {
  .Call(export_adj_following_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_preceding <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get_events()

  adj_preceding_impl(x, events)
}

adj_preceding_impl <- function(x, events) {
  .Call(export_adj_preceding_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_modified_following <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get_events()

  adj_modified_following_impl(x, events)
}

adj_modified_following_impl <- function(x, events) {
  .Call(export_adj_modified_following_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_modified_preceding <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get_events()

  adj_modified_preceding_impl(x, events)
}

adj_modified_preceding_impl <- function(x, events) {
  .Call(export_adj_modified_preceding_impl, x, events)
}

#' @rdname adjustments
#' @export
adj_nearest <- function(x, schedule) {
  x <- vec_cast_date(x)
  schedule <- as_schedule(schedule)

  events <- schedule$cache$get_events()

  adj_nearest_impl(x, events)
}

adj_nearest_impl <- function(x, events) {
  .Call(export_adj_nearest_impl, x, events)
}
