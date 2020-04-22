#' Create a new recurrence bundle
#'
#' @description
#' Often, a single recurrence rule created from a base rule like `monthly()`
#' will be sufficient. However, more complex rules can be constructed
#' by combining simple rules into a _recurrence bundle_.
#'
#' `rbundle()` creates a new empty recurrence bundle.
#'
#' - Add recurrence rules or other recurrence bundles with [add_rschedule()].
#'
#' - Forcibly include dates with [add_rdate()].
#'
#' - Forcibly exclude dates with [add_exdate()].
#'
#' @return
#' An empty rbundle.
#'
#' @seealso [add_rschedule()]
#' @export
#' @examples
#' rbundle()
#'
#' on_weekends <- weekly() %>%
#'   recur_on_weekends()
#'
#' rbundle() %>%
#'   add_rschedule(on_weekends)
rbundle <- function() {
  new_rbundle()
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rbundle <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
print.rbundle <- function(x, ...) {
  fmt <- format(x)
  print(glue("<rbundle[{fmt}]>"))
  invisible(x)
}

#' @export
format.rbundle <- function(x, ...) {
  n_rschedules <- length(x$rschedules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("{n_rschedules} rschedules / {n_rdates} rdates / {n_exdates} exdates")
}

# ------------------------------------------------------------------------------

new_rbundle <- function(rschedules = list(),
                        rdates = new_date(),
                        exdates = new_date()) {

  if (!is_list(rschedules)) {
    abort("`rschedules` must be a list.")
  }

  if (!is_date(rdates)) {
    abort("`rdates` must be a Date.")
  }

  if (!is_date(exdates)) {
    abort("`exdates` must be a Date.")
  }

  cache <- cache_rbundle$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  data <- list(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache
  )

  new_rschedule(data, class = "rbundle")
}

# ------------------------------------------------------------------------------

#' Is `x` a recurrence bundle?
#'
#' `is_rbundle()` tests if `x` is a recurrence bundle.
#'
#' @param x `[object]`
#'
#'   An object.
#'
#' @return
#' `TRUE` if `x` inherits from `"rbundle"`, otherwise `FALSE`.
#'
#' @export
#' @examples
#' is_rbundle(rbundle())
is_rbundle <- function(x) {
  inherits(x, "rbundle")
}

# ------------------------------------------------------------------------------

validate_rbundle <- function(x, x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (!is_rbundle(x)) {
    glubort("Input{x_arg} must be an rbundle.")
  }

  invisible(x)
}
