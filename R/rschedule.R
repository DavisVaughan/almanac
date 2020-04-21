# This is an abstract class that `rrule`, `rbundle` and `calendar` all inherit
# from to declare that they have a `x$cache` element that can be used to extract
# events with `x$cache$get_events()`. This is used by `alma_*()` functions.

new_rschedule <- function(data, class = character()) {
  structure(data, class = c(class, "rschedule"))
}

# ------------------------------------------------------------------------------

rschedule_events <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

is_rschedule <- function(x) {
  inherits(x, "rschedule")
}

validate_rschedule <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_rschedule(x)) {
    glubort("Input{x_arg} must be an rschedule, such as an rrule or rbundle.")
  }

  invisible(x)
}

