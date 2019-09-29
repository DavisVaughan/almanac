#' @export
rr_on_yday <- function(x, day) {
  validate_rrule(x)

  old <- get_rule(x, "yday")
  new <- vec_cast(day, integer(), x_arg = "day")

  abs_new <- abs(new)
  if (any(abs_new > 366 | abs_new < 1)) {
    abort("`n` can only take values in [-366, -1] and [1, 366].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, yday = new)
}
