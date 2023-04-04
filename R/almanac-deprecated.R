#' Deprecated recurrence helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `recur_on_mday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_month()].
#'
#' @inheritParams recur_on_day_of_month
#' @inheritParams recur_on_day_of_week
#'
#' @param mday `[integer]`
#'
#'    The days of the month on which to recur. Negative values are allowed,
#'    which specify `n` days from the end of the month.
#'
#' @param wday `[integer / character]`
#'
#'    Days of the week to recur on. Integer values must be from `1` to `7`, with
#'    `1 = Monday` and `7 = Sunday`. This is also allowed to be a full weekday
#'    string like `"Tuesday"`, or an abbreviation like `"Tues"`.
#'
#' @keywords internal
#' @name deprecated-recur
NULL

#' @rdname deprecated-recur
#' @export
recur_on_mday <- function(x, mday) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_mday()",
    with = "recur_on_day_of_month()",
    always = TRUE
  )
  recur_on_day_of_month(x = x, day = mday)
}

#' @rdname deprecated-recur
#' @export
recur_on_wday <- function(x, wday, nth = NULL) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_wday()",
    with = "recur_on_day_of_week()",
    always = TRUE
  )
  recur_on_day_of_week(x = x, day = wday, nth = nth)
}
