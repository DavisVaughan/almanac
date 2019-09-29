rr_position <- function(x, n) {
  validate_rrule(x)

  if (is_already_set(x, "position")) {
    abort("`position` has already been set for this rrule.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")

  # TODO - Limit range of `n`? 366 max?

  tweak_rrule(x, position = n)
}
