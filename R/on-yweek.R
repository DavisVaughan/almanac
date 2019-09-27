#' @export
on_yweek <- function(x, n) {
  validate_rrule(x)

  old <- get_rule(x, "yweek")
  new <- vec_cast(n, integer(), x_arg = "n")

  abs_new <- abs(new)
  if (any(abs_new > 53 | abs_new < 1)) {
    abort("`n` can only take values in [-53, -1] and [1, 53].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, yweek = new)
}
