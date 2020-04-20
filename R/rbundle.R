#' Create a new recurrence bundle
#'
#' @description
#'
#' Often, a single recurrence rule created from a base rule like `monthly()`
#' will be sufficient. However, more complex rules can be constructed
#' by combining simple rules into a _recurrence bundle_.
#'
#' `rbundle()` creates a new empty recurrence bundle. Add recurrence rules to
#' the bundle with [add_rrule()]. Add required dates with [add_rdate()].
#'
#' @return
#' An empty rbundle.
#'
#' @seealso [add_rrule()]
#' @export
#' @examples
#' rbundle()
#' add_rrule(rbundle(), monthly())
rbundle <- function() {
  new_rbundle()
}

# ------------------------------------------------------------------------------

#' @export
print.rbundle <- function(x, ...) {
  cat(glue("rbundle: {rbundle_summary(x)}"))
  invisible(x)
}

rbundle_summary <- function(x) {
  n_rrules <- length(x$rrules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("{n_rrules} rrules / {n_rdates} rdates / {n_exdates} exdates")
}

# ------------------------------------------------------------------------------

new_rbundle <- function(rrules = list(),
                        rdates = new_date(),
                        exdates = new_date(),
                        cache = Cache$new()) {
  cache$set_rrules(rrules)
  cache$set_rdates(rdates)
  cache$set_exdates(exdates)

  data <- list(
    rrules = rrules,
    rdates = rdates,
    exdates = exdates,
    cache = cache
  )

  structure(data, class = "rbundle")
}

# ------------------------------------------------------------------------------

as_rbundle <- function(x, ...) {
  UseMethod("as_rbundle")
}

as_rbundle.default <- function(x, ...) {
  abort(glue("Cannot convert {class(x)[1]} to a rbundle."))
}

# Use the same cache as the `rrule`. Generally useful
# when a user creates a rrule then passes it into a function
# like `alma_search()`, which converts it to a rbundle. We want
# to update the cache of the original rrule.
as_rbundle.rrule <- function(x, ...) {
  new_rbundle(rrules = list(x), cache = x$cache)
}

as_rbundle.rbundle <- function(x, ...) {
  x
}

# ------------------------------------------------------------------------------

is_rbundle <- function(x) {
  inherits(x, "rbundle")
}

validate_rbundle <- function(x, arg = "`x`") {
  if (!is_rbundle(x)) {
    glubort("{arg} must be an rbundle.")
  }

  invisible(x)
}
