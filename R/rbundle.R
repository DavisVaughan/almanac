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
#' - `add_exdates()`
#'
#' - `add_rschedule()`
#'
#' @details
#' An rbundle is an abstract class that rintersect, runion, and rsetdiff all
#' inherit from. The sole purpose of an rbundle subclass is to implement an
#' `rbundle_restore()` method that defines how to recover the original
#' rbundle subclass after adding a new rschedule, rdate, or exdate.
#' Additionally, because rbundles are also rschedules, a [rschedule_events()]
#' method must be implemented.
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
#'   Additional named elements added to the rbundle object.
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
#'   An rbundle subclass that you are restoring to.
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
    check_rschedule(rschedules[[i]], arg = cli::format_inline("rschedules[[{i}]]"))
  }

  if (!is_date(rdates)) {
    abort("`rdates` must be a Date.")
  }
  if (any(is_missing_or_infinite(rdates))) {
    abort("`rdates` must be finite.")
  }
  check_date_within_bounds(rdates)

  if (!is_date(exdates)) {
    abort("`exdates` must be a Date.")
  }
  if (any(is_missing_or_infinite(exdates))) {
    abort("`exdates` must be finite.")
  }
  check_date_within_bounds(exdates)

  new_rschedule(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    ...,
    class = c(class, "rbundle")
  )
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
  glubort("rbundle subclasses must provide their own `rbundle_restore()` method.")
}

# ------------------------------------------------------------------------------

is_rbundle <- function(x) {
  inherits(x, "rbundle")
}

check_rbundle <- function(x,
                          ...,
                          allow_null = FALSE,
                          arg = caller_arg(x),
                          call = caller_env()) {
  check_inherits(
    x = x,
    what = "rbundle",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
