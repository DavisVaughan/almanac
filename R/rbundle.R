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
                        exdates = new_date()) {
  cache = cache_rbundle$new()
  cache$set_rrules(rrules)
  cache$set_rdates(rdates)
  cache$set_exdates(exdates)

  data <- list(
    rrules = rrules,
    rdates = rdates,
    exdates = exdates,
    cache = cache
  )

  new_cacher(data, class = "rbundle")
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

validate_rbundle <- function(x, arg = "`x`") {
  if (!is_rbundle(x)) {
    glubort("{arg} must be an rbundle.")
  }

  invisible(x)
}
