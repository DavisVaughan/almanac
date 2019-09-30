#' @export
rr_until <- function(x, until) {
  validate_rrule(x)

  if (is_already_set(x, "until")) {
    abort("`until` has already been set for this rrule.")
  }

  until <- vec_cast_date(until, "until")
  vec_assert(until, size = 1L)

  x$rules$since

  if (until < x$rules$since) {
    abort("`until` must be greater than `since`.")
  }

  tweak_rrule(x, until = until)
}
