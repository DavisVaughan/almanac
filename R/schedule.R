#' @export
schedule <- function() {
  new_schedule()
}

#' @export
add_rrule <- function(x, rrule) {
  validate_schedule(x)
  validate_rrule(rrule, arg = "`rrule`")
  x$recurrences$rrules <- c(x$recurrences$rrules, list(rrule))
  x
}

#' @export
add_rdate <- function(x, rdate) {
  validate_schedule(x)
  rdate <- vec_cast_date(rdate, "rdate")
  x$recurrences$rdates <- c(x$recurrences$rdates, list(rdate))
  x
}

#' @export
add_exdate <- function(x, exdate) {
  validate_schedule(x)
  exdate <- vec_cast_date(exdate, "exdate")
  x$recurrences$exdates <- c(x$recurrences$exdates, list(exdate))
  x
}

#' @export
add_schedule <- function(x, schedule) {
  validate_schedule(x)
  validate_schedule(schedule, "`schedule`")

  x_recurrences <- x$recurrences
  y_recurrences <- schedule$recurrences

  new_schedule(
    rrules = c(x_recurrences$rrules, y_recurrences$rrules),
    rdates = c(x_recurrences$rdates, y_recurrences$rdates),
    exdates = c(x_recurrences$exdates, y_recurrences$exdates)
  )
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

init_schedule <- function(x) {
  # Only initialize once
  if (!is.null(get_context(x))) {
    return()
  }

  init_context(x)

  context <- get_context(x)
  recurrences <- x$recurrences

  v8_eval(context, "var ruleset = new rrule.RRuleSet()")

  for(rrule in recurrences$rrules) {
    rrule <- as_js_from_rrule(rrule, context)
    v8_eval(context, "ruleset.rrule([[rrule]])")
  }

  for(rdate in recurrences$rdates) {
    rdate <- as_js_from_date(rdate)
    v8_eval(context, "ruleset.rdate([[rdate]])")
  }

  for(exdate in recurrences$exdates) {
    exdate <- as_js_from_date(exdate)
    v8_eval(context, "ruleset.exdate([[exdate]])")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

init_context <- function(x) {
  context <- V8::v8()

  source_rrule_js(context)

  x[["env"]][["context"]] <- context

  invisible(x)
}

get_context <- function(x) {
  x[["env"]][["context"]]
}

source_rrule_js <- function(x) {
  x$source(rrule_js_path)
}

# ------------------------------------------------------------------------------

v8_eval <- function(x, ..., .envir = parent.frame()) {
  x$eval(glue2(..., .envir = .envir))
}

v8_assign <- function(x, name, value) {
  x$assign(name, value)
}

v8_get <- function(x, ..., .envir = parent.frame()) {
  x$get(glue2(..., .envir = .envir))
}
