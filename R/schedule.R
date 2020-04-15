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

new_schedule <- function(rrules = list(), rdates = new_date(), exdates = new_date(), env = NULL) {
  recurrences <- list(
    rrules = rrules,
    rdates = rdates,
    exdates = exdates
  )

  if (is.null(env)) {
    env <- new.env(parent = emptyenv())
    env[["initialized"]] <- FALSE
  }

  data <- list(
    recurrences = recurrences,
    env = env
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

as_schedule.rrule <- function(x, ...) {
  new_schedule(rrules = list(x), env = x$env)
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

# cache is always set with dates generated from an inclusive between

cache_set <- function(schedule, to, events) {
  env <- schedule[["env"]]

  # No previous cache
  if (is.null(env[["events"]])) {
    env[["to"]] <- to
    env[["events"]] <- events
    return(invisible(schedule))
  }

  numeric_to <- unclass(to)
  old_numeric_to <- unclass(env[["to"]])

  needs_new_events <- FALSE

  if (old_numeric_to < numeric_to) {
    env[["to"]] <- to
    needs_new_events <- TRUE
  }

  if (needs_new_events) {
    env[["events"]] <- events
  }

  invisible(schedule)
}

cache_get <- function(schedule, from, to, inclusive) {
  env <- schedule[["env"]]
  events <- env[["events"]]

  # No cache
  if (is.null(events)) {
    return(NULL)
  }

  numeric_to <- unclass(to)
  env_numeric_to <- unclass(env[["to"]])

  # After end of cache
  if (env_numeric_to < numeric_to) {
    return(NULL)
  }

  numeric_from <- unclass(from)
  numeric_events <- unclass(events)

  # Cache is always stored inclusively, so these events exist
  if (inclusive) {
    locs <- numeric_events >= numeric_from & numeric_events <= numeric_to
  } else {
    locs <- numeric_events > numeric_from & numeric_events < numeric_to
  }

  events <- events[locs]

  events
}

# ------------------------------------------------------------------------------

get_schedule_since <- function(x) {
  pull_since <- function(x) {
    x$rules$since
  }

  since <- get_rrules_since(x)

  rdates <- x$recurrences$rdates

  if (length(rdates) == 0L) {
    return(since)
  }

  since <- min(rdates, since)

  since
}

# Minimum `since` date of all rules
get_rrules_since <- function(x) {
  rrules <- x$recurrences$rrules

  if (length(rrules) == 0L) {
    return(new_date())
  }

  pull_since <- function(x) {
    x$rules$since
  }

  since <- min(vapply(rrules, pull_since, numeric(1)))
  class(since) <- "Date"

  since
}

sch_since <- function(x) {
  x[["env"]][["since"]]
}

# ------------------------------------------------------------------------------

init_schedule <- function(x) {
  if (x$env$initialized) {
    return()
  }

  recurrences <- x$recurrences

  v8_eval("var ruleset = new rrule.RRuleSet()")

  for(rrule in recurrences$rrules) {
    rrule <- as_js_from_rrule(rrule)
    v8_eval("ruleset.rrule([[rrule]])")
  }

  rdates <- recurrences$rdates

  for(i in seq_along(rdates)) {
    rdate <- as_js_from_date(rdates[i])
    v8_eval("ruleset.rdate([[rdate]])")
  }

  exdates <- recurrences$exdates

  for(i in seq_along(exdates)) {
    exdate <- as_js_from_date(exdates[i])
    v8_eval("ruleset.exdate([[exdate]])")
  }

  x$env[["initialized"]] <- TRUE

  x$env[["since"]] <- get_schedule_since(x)

  x$env[["n_rrules"]] <- length(recurrences$rrules)
  x$env[["n_rdates"]] <- length(recurrences$rdates)
  x$env[["n_exdates"]] <- length(recurrences$exdates)

  invisible(x)
}

# Only for use after a schedule has been initialized
sch_has_rrules_or_rdates <- function(x) {
  sum(x$env[["n_rrules"]], x$env[["n_rdates"]]) != 0L
}

# ------------------------------------------------------------------------------

v8_eval <- function(..., .envir = parent.frame()) {
  almanac_global_context$eval(glue2(..., .envir = .envir))
}

v8_assign <- function(name, value) {
  almanac_global_context$assign(name, value)
}

v8_get <- function(..., .envir = parent.frame()) {
  almanac_global_context$get(glue2(..., .envir = .envir))
}
