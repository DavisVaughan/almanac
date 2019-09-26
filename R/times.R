times <- function(x, n) {
  if (is_already_set(x, "times")) {
    abort("`times` has already been set for this rrule.")
  }

  n <- vec_cast(n, integer(), x_arg = "n")
  vec_assert(n, size = 1L)

  if (n <= 0L) {
    abort("`n` must be greater than 0.")
  }

  tweak_rrule(x, times = n)
}
