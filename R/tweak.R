# Construct a completely new rrule object, with a different context environment

tweak_rrule <- function(x, ...) {
  new <- list2(...)
  old <- x$rules
  old[names(new)] <- new
  exec(new_rrule, !!!old)
}
