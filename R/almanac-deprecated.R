#' Deprecated recurrence helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `recur_on_mday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_month()].
#'
#' - `recur_on_wday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_week()].
#'
#' - `recur_on_yday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_year()].
#'
#' - `recur_on_yweek()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_week_of_year()].
#'
#' - `recur_on_ymonth()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_month_of_year()].
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
#' @param yweek `[integer]`
#'
#'    Weeks of the year to recur on. Integer values must be between
#'    `[1, 53]` or `[-53, -1]`.
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

#' @rdname deprecated-recur
#' @export
recur_on_yday <- function(x, yday) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_yday()",
    with = "recur_on_day_of_year()",
    always = TRUE
  )
  recur_on_day_of_year(x = x, day = yday)
}

#' @rdname deprecated-recur
#' @export
recur_on_yweek <- function(x, yweek) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_yweek()",
    with = "recur_on_week_of_year()",
    always = TRUE
  )
  recur_on_week_of_year(x = x, week = yweek)
}

#' @rdname deprecated-recur
#' @export
recur_on_ymonth <- function(x, ymonth) {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "recur_on_ymonth()",
    with = "recur_on_month_of_year()",
    always = TRUE
  )
  recur_on_month_of_year(x = x, month = ymonth)
}
