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
#' An rschedule is an abstract class that rrule and rset both inherit from.
#' The sole functionality of rschedule classes is to provide a method for
#' `rschedule_events()`.
#'
#' @param ... `[named fields]`
#'
#'   Named data fields.
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
#'   events = events,
#'   class = "static_rschedule"
#' )
#'
#' # You have to register an `rschedule_events()` method first!
#' try(alma_events(static))
new_rschedule <- function(..., class) {
  data <- list(...)

  if (length(data) == 0L) {
    abort("`...` must have at least one field.")
  }

  if (!is_named(data)) {
    abort("`...` must have named elements.")
  }

  structure(data, class = c(class, "almanac_rschedule"))
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
  cli::cli_abort("Can't extract events from a <{cls}>.")
}

#' @export
rschedule_events.almanac_rschedule <- function(x) {
  cli::cli_abort(
    "<almanac_rschedule> subclasses must provide their own `rschedule_events()` method."
  )
}

# ------------------------------------------------------------------------------

is_rschedule <- function(x) {
  inherits(x, "almanac_rschedule")
}

check_rschedule <- function(
  x,
  ...,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(
    x = x,
    what = "almanac_rschedule",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

list_check_all_rschedules <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  for (i in seq_along(x)) {
    check_rschedule(
      x = x[[i]],
      arg = cli::format_inline("{arg}[[{i}]]"),
      call = call
    )
  }
  invisible(NULL)
}
