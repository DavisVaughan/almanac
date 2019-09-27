#' @export
on_mday <- function(x, n) {
  validate_rrule(x)

  old <- get_rule(x, "mday")
  new <- vec_cast(n, integer(), x_arg = "n")

  abs_new <- abs(new)
  if (any(abs_new > 31 | abs_new < 1)) {
    abort("`n` can only take values in [-31, -1] and [1, 31].")
  }

  new <- union(old, new)
  new <- unique(new)
  new <- sort(new)

  tweak_rrule(x, mday = new)
}
