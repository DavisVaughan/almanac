#' Create a new rschedule
#'
#' @description
#' `new_rschedule()` is a developer focused tool that is not required for
#' normal usage of almanac. It is only exported to allow other packages
#' to construct new rschedule objects that work with almanac functions
#' prefixed with `alma_*()`, like [alma_in()].
#'
#' `rschedule_events()` is a generic function that rschedule subclasses must
#' provide a method for. `rschedule_events()` should return a Date vector
#' containing the complete ordered set of events in the event set of
#' that rschedule.
#'
#' @details
#' An rschedule is an abstract class that rrule and rbundle both inherit from.
#' The sole functionality of rschedule classes is to provide a method for
#' `rschedule_events()`.
#'
#' @param data `[named list]`
#'
#'   A named list of data fields.
#'
#' @param class `[character]`
#'
#'   A required subclass.
#'
#' @param x `[rschedule subclass]`
#'
#'   An object that subclasses rschedule.
#'
#' @return
#' For `new_rschedule()`, a new rschedule subclass.
#'
#' For `rschedule_events()`, a Date vector of events.
#'
#' @export
#' @examples
#' events <- as.Date("1970-01-01")
#'
#' static <- new_rschedule(
#'   list(events = events),
#'   class = "static_rschedule"
#' )
#'
#' rschedule_events.static_rschedule <- function(x) {
#'   x$events
#' }
#'
#' alma_events(static)
#'
#' alma_in(events, static)
#' alma_in(events + 1, static)
new_rschedule <- function(data, class) {
  if (!is_list(data)) {
    abort("`data` must be a list.")
  }
  if (length(data) == 0L) {
    abort("`data` must have at least one field.")
  }
  if (!is_named(data)) {
    abort("`data` must have named elements.")
  }

  structure(data, class = c(class, "rschedule"))
}

# ------------------------------------------------------------------------------

#' @rdname new_rschedule
#' @export
rschedule_events <- function(x) {
  UseMethod("rschedule_events")
}

#' @export
rschedule_events.default <- function(x) {
  cls <- glue::glue_collapse(class(x), sep = "/")
  glubort("Cannot extract events from an object of class <{cls}>.")
}

#' @export
rschedule_events.rschedule <- function(x) {
  glubort("rschedule subclasses must provide their own `rschedule_events()` method.")
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

