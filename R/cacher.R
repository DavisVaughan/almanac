# This is an abstract class that `rrule`, `rbundle` and `calendar` all inherit
# from to declare that they have a `x$cache` element that can be used to extract
# events with `x$cache$get_events()`. This is used by `alma_*()` functions.

new_cacher <- function(data, class = character()) {
  structure(data, class = c(class, "cacher"))
}

# ------------------------------------------------------------------------------

cacher_events <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

as_cacher <- function(x, ...) {
  UseMethod("as_cacher")
}

as_cacher.default <- function(x, ...) {
  abort(glue("Cannot convert {class(x)[1]} to an rrule, rbundle, or calendar."))
}

as_cacher.cacher <- function(x, ...) {
  x
}
