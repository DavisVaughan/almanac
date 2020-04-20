# This is an abstract class that both `rbundle` and `calendar` inherit from
# to declare that they have a `x$cache` element that can be used to extract
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
  abort(glue("Cannot convert {class(x)[1]} to an rbundle or calendar."))
}

as_cacher.cacher <- function(x, ...) {
  x
}

# Use the same cache as the `rrule`. Generally useful
# when a user creates a rrule then passes it into a function
# like `alma_search()`, which converts it to a rbundle. We want
# to update the cache of the original rrule.
as_cacher.rrule <- function(x, ...) {
  new_rbundle(rrules = list(x), cache = x$cache)
}

# ------------------------------------------------------------------------------

is_cacher <- function(x) {
  inherits(x, "cacher")
}

validate_cacher <- function(x, x_arg = "x") {
  if (!is_cacher(x)) {
    abort("`{x_arg}` must be a rbundle or calendar.")
  }
}
