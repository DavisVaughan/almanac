#' Create a new schedule
#'
#' @description
#'
#' Often, a single recurrence rule created from a base rule like `monthly()`
#' will be sufficient. However, more complex rules can be constructed
#' by combining simple rules into a _schedule_.
#'
#' `schedule()` creates a new empty schedule. Add rules to the schedule with
#' [sch_add_rrule()].
#'
#' @examples
#' schedule()
#'
#' sch_add_rrule(schedule(), monthly())
#'
#' @seealso [sch_add_rrule()]
#' @export
schedule <- function() {
  new_schedule()
}

# ------------------------------------------------------------------------------

new_schedule <- function(rrules = list(), rdates = list(), exdates = list(), env = NULL) {
  recurrences <- list(
    rrules = rrules,
    rdates = rdates,
    exdates = exdates
  )

  if (is.null(env)) {
    env <- new.env(parent = emptyenv())
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

cache_set <- function(schedule, from, to, events) {
  env <- schedule[["env"]]

  numeric_from <- as.numeric(from)
  numeric_to <- as.numeric(to)

  # No previous cache
  if (is.null(env[["events"]])) {
    env[["from"]] <- from
    env[["to"]] <- to
    env[["events"]] <- events
    env[["numeric_from"]] <- numeric_from
    env[["numeric_to"]] <- numeric_to
    env[["numeric_events"]] <- as.numeric(events)
    return(invisible(schedule))
  }

  old_numeric_from <- env[["numeric_from"]]
  old_numeric_to <- env[["numeric_to"]]
  old_numeric_events <- env[["numeric_events"]]

  needs_new_events <- FALSE

  if (old_numeric_from > numeric_from) {
    new_from <- from
    new_numeric_from <- numeric_from
    needs_new_events <- TRUE
  } else {
    new_from <- env[["from"]]
    new_numeric_from <- old_numeric_from
  }

  if (old_numeric_to < numeric_to) {
    new_to <- to
    new_numeric_to <- numeric_to
    needs_new_events <- TRUE
  } else {
    new_to <- env[["to"]]
    new_numeric_to <- old_numeric_to
  }

  if (needs_new_events) {
    numeric_events <- as.numeric(events)
    new_numeric_events <- unique(sort(c(old_numeric_events, numeric_events)))
    new_events <- .Date(new_numeric_events)
  } else {
    new_numeric_events <- old_numeric_events
    new_events <- env[["events"]]
  }

  env[["from"]] <- new_from
  env[["to"]] <- new_to
  env[["events"]] <- new_events

  env[["numeric_from"]] <- new_numeric_from
  env[["numeric_to"]] <- new_numeric_to
  env[["numeric_events"]] <- new_numeric_events

  invisible(schedule)
}

cache_get <- function(schedule, from, to, inclusive) {
  env <- schedule[["env"]]
  events <- env[["events"]]

  # No cache
  if (is.null(events)) {
    return(NULL)
  }

  numeric_from <- as.numeric(from)
  env_numeric_from <- env[["numeric_from"]]

  # Before start of cache
  if (env_numeric_from > numeric_from) {
    return(NULL)
  }

  numeric_to <- as.numeric(to)
  env_numeric_to <- env[["numeric_to"]]

  # After end of cache
  if (env_numeric_to < numeric_to) {
    return(NULL)
  }

  numeric_events <- env[["numeric_events"]]

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

init_schedule <- function(x) {
  recurrences <- x$recurrences

  v8_eval("var ruleset = new rrule.RRuleSet()")

  for(rrule in recurrences$rrules) {
    rrule <- as_js_from_rrule(rrule)
    v8_eval("ruleset.rrule([[rrule]])")
  }

  for(rdate in recurrences$rdates) {
    rdate <- as_js_from_date(rdate)
    v8_eval("ruleset.rdate([[rdate]])")
  }

  for(exdate in recurrences$exdates) {
    exdate <- as_js_from_date(exdate)
    v8_eval("ruleset.exdate([[exdate]])")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

v8_eval <- function(..., .envir = parent.frame()) {
  global_context$eval(glue2(..., .envir = .envir))
}

v8_assign <- function(name, value) {
  global_context$assign(name, value)
}

v8_get <- function(..., .envir = parent.frame()) {
  global_context$get(glue2(..., .envir = .envir))
}
