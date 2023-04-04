#' Deprecated recurrence helpers
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' - `recur_on_mday()` is deprecated as of almanac 1.0.0 in favor of
#'   [recur_on_day_of_month()].
#'
#' @inheritParams recur_on_day_of_month
#'
#' @param mday `[integer]`
#'
#'    The days of the month on which to recur. Negative values are allowed,
#'    which specify `n` days from the end of the month.
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
