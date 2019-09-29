#' @export
rr_on_ymonth <- function(x, month) {
  validate_rrule(x)
  month <- month_normalize(month)

  old <- get_rule(x, "ymonth")
  new <- vec_cast(month, integer(), x_arg = "month")

  if (any(new > 12 | new < 1)) {
    abort("`n` can only take values in [1, 12].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, ymonth = new)
}
