# `n` is an offset around easter as long as the day still falls in the same year
# Meaning that if you set `n = -103` it won't roll back to the previous year in
# some cases. You just won't get a value.

#' @export
rr_on_easter <- function(x, n = 0L) {
  validate_rrule(x)

  if (is_already_set(x, "easter")) {
    abort("The `easter` rule has already been set.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")
  vec_assert(n, size = 1L, arg = "n")

  abs_n <- abs(n)
  if (abs_n > 366) {
    abort("`n` can only take values in [-366, 366].")
  }

  tweak_rrule(x, easter = n)
}
