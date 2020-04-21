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

is_cacher <- function(x) {
  inherits(x, "cacher")
}

validate_cacher <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_cacher(x)) {
    glubort("Input{x_arg} must be a rrule, rbundle, or calendar.")
  }

  invisible(x)
}

