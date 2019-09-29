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
