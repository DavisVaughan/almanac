#' Create a new schedule
#'
#' @description
#'
#' Often, a single recurrence rule created from a base rule like `monthly()`
#' will be sufficient. However, more complex rules can be constructed
#' by combining simple rules into a _schedule_.
#'
#' `schedule()` creates a new empty schedule. Add rules to the schedule with
#' [sch_rrule()].
#'
#' @examples
#' schedule()
#'
#' sch_rrule(schedule(), monthly())
#'
#' @seealso [sch_rrule()]
#' @export
schedule <- function() {
  new_schedule()
}

# ------------------------------------------------------------------------------

#' @export
print.schedule <- function(x, ...) {
  cat(glue("schedule: {sch_summary(x)}"))
  invisible(x)
}

sch_summary <- function(x) {
  recurrences <- x$recurrences

  n_rrules <- length(recurrences$rrules)
  n_rdates <- length(recurrences$rdates)
  n_exdates <-length(recurrences$exdates)

  glue("{n_rrules} rrules / {n_rdates} rdates / {n_exdates} exdates")
}

# ------------------------------------------------------------------------------

new_schedule <- function(rrules = list(),
                         rdates = new_date(),
                         exdates = new_date(),
                         cache = Cache$new()) {
  recurrences <- new_recurrences(
    rrules = rrules,
    rdates = rdates,
    exdates = exdates
  )

  cache$set_recurrences(recurrences)

  data <- list(
    recurrences = recurrences,
    cache = cache
  )

  structure(data, class = "schedule")
}

# ------------------------------------------------------------------------------

as_schedule <- function(x, ...) {
  UseMethod("as_schedule")
}

as_schedule.default <- function(x, ...) {
  abort(glue("Cannot convert {class(x)[1]} to a schedule."))
}

# Use the same cache as the `rrule`. Generally useful
# when a user creates a rrule then passes it into a function
# like `alma_seq()`, which converts it to a schedule. We want
# to update the cache of the original rrule.
as_schedule.rrule <- function(x, ...) {
  new_schedule(rrules = list(x), cache = x$cache)
}

as_schedule.schedule <- function(x, ...) {
  x
}

# ------------------------------------------------------------------------------

is_schedule <- function(x) {
  inherits(x, "schedule")
}

validate_schedule <- function(x, arg = "`x`") {
  if (!is_schedule(x)) {
    glubort("{arg} must be a schedule.")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

new_recurrences <- function(rrules = list(),
                            rdates = new_date(),
                            exdates = new_date()) {
  list(rrules = rrules, rdates = rdates, exdates = exdates)
}
