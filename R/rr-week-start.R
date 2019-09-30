#' Control the start of the week
#'
#' @description
#'
#' `rr_week_start()` controls the week day that represents the start of the
#' week. This is important for rules that use [rr_on_yweek()]. See that page
#' for examples.
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
#' @export
rr_week_start <- function(x, wday) {
  validate_rrule(x)

  wday <- wday_normalize(wday)

  wday <- vec_cast(wday, integer(), x_arg = "wday")
  vec_assert(wday, size = 1L)

  if (wday < 1L || wday > 7L) {
    abort("`wday` must be an integer between 1 and 7.")
  }

  tweak_rrule(x, week_start = wday)
}
