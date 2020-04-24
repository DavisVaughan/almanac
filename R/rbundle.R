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
#' - Forcibly include dates with [add_rdates()].
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
  print(format(x))
  invisible(x)
}

#' @export
format.rbundle <- function(x, ...) {
  n_rschedules <- length(x$rschedules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("<rbundle[{n_rschedules} rschedules / {n_rdates} rdates / {n_exdates} exdates]>")
}

# ------------------------------------------------------------------------------

#' Constructor for an rbundle
#'
#' @description
#' `new_rbundle()` is a developer focused tool that is not required for normal
#' usage of almanac. It constructs a new rbundle directly from a list of
#' existing rschedules.
#'
#' `rbundle_restore()` is a generic function that rbundle subclasses can provide
#' a method for. It dispatches off of `to`. Its sole purpose is to restore
#' classes and fields of the subclass after calling any of the following
#' functions:
#'
#' - `add_rdates()`
#'
#' - `add_exdate()`
#'
#' - `add_rschedule()`
#'
#' @param rschedules `[list]`
#'
#'   A list of rschedules.
#'
#' @param rdates `[Date]`
#'
#'   A vector of dates to forcibly include in the event set.
#'
#' @param exdates `[Date]`
#'
#'   A vector of dates to forcibly exclude from the event set.
#'
#' @param ... `[named dots]`
#'
#'   Additional named elements added to the rbundle list.
#'
#' @param class `[character]`
#'
#'   An optional subclass.
#'
#' @param x `[rbundle]`
#'
#'   An updated rbundle that needs to be restored to the type of `to`.
#'
#' @param to `[rbundle subclass]`
#'
#'   An rbundle, or an rbundle subclass, that you are restoring to.
#'
#' @return
#' - `new_rbundle()` returns a new rbundle.
#'
#' - `rbundle_restore()` should return an rbundle subclass of the same type
#'   as `to`.
#'
#' @export
#' @examples
#' new_rbundle()
#'
#' x <- daily()
#' y <- weekly()
#'
#' rschedules <- list(x, y)
#'
#' new_rbundle(rschedules)
new_rbundle <- function(rschedules = list(),
                        rdates = new_date(),
                        exdates = new_date(),
                        ...,
                        class = character()) {

  if (!is_list(rschedules)) {
    abort("`rschedules` must be a list.")
  }

  for (i in seq_along(rschedules)) {
    validate_rschedule(rschedules[[i]], x_arg = glue("rschedules[[{i}]]"))
  }

  if (!is_date(rdates)) {
    abort("`rdates` must be a Date.")
  }
  if (any(is_missing_or_infinite(rdates))) {
    abort("`rdates` must be finite.")
  }
  validate_date_bounds(rdates, x_arg = "rdates")

  if (!is_date(exdates)) {
    abort("`exdates` must be a Date.")
  }
  if (any(is_missing_or_infinite(exdates))) {
    abort("`exdates` must be finite.")
  }
  validate_date_bounds(exdates, x_arg = "exdates")

  cache <- cache_rbundle$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  data <- list(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...
  )

  if (!is_named(data)) {
    abort("All elements of `...` must be named.")
  }

  new_rschedule(data, class = c(class, "rbundle"))
}

# ------------------------------------------------------------------------------

#' @rdname new_rbundle
#' @export
rbundle_restore <- function(x, to) {
  UseMethod("rbundle_restore", to)
}

#' @export
rbundle_restore.default <- function(x, to) {
  cls <- glue::glue_collapse(class(to), sep = "/")
  glubort("Can't restore an rbundle to a <{cls}>.")
}

#' @export
rbundle_restore.rbundle <- function(x, to) {
  x
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
