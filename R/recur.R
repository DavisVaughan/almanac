daily <- function(since = "1970-01-01") {
  rrule(since, frequency = "daily")
}

weekly <- function(since = "1970-01-01") {
  rrule(since, frequency = "weekly")
}

monthly <- function(since = "1970-01-01") {
  rrule(since, frequency = "monthly")
}

yearly <- function(since = "1970-01-01") {
  rrule(since, frequency = "yearly")
}

rrule <- function(since = "1970-01-01", frequency = "yearly") {
  since <- vec_cast_date(since, "since")

  new_rrule(
    since = since,
    frequency = frequency
  )
}

new_rrule <- function(since = as.Date("1970-01-01"),
                      frequency = "yearly",
                      until = NULL,
                      times = NULL,
                      interval = 1L,
                      week_start = 1L,
                      ymonth = NULL,
                      yweek = NULL,
                      yday = NULL,
                      mday = NULL,
                      wday = NULL,
                      set_pos = NULL) {

  env <- new.env(parent = emptyenv())

  rules <- list(
    since = since,
    frequency = frequency,
    until = until,
    times = times,
    interval = interval,
    week_start = week_start,
    ymonth = ymonth,
    yweek = yweek,
    yday = yday,
    mday = mday,
    wday = wday,
    set_pos = set_pos
  )

  data <- list(
    rules = rules,
    env = env
  )

  structure(data, class = "rrule")
}

init_rrule <- function(x) {
  # Only initialize once
  if (!is.null(get_context(x))) {
    return()
  }

  init_context(x)

  context <- get_context(x)
  rrule <- as_js_rrule(x)

  v8_eval(context, "var rule = [[rrule]]")
}

init_context <- function(x) {
  context <- V8::v8()

  source_rrule_js(context)

  x[["env"]][["context"]] <- context

  invisible(x)
}

get_context <- function(x) {
  x[["env"]][["context"]]
}

v8_eval <- function(x, ..., .envir = parent.frame()) {
  x$eval(glue2(..., .envir = .envir))
}

v8_assign <- function(x, name, value) {
  x$assign(name, value)
}

v8_get <- function(x, ..., .envir = parent.frame()) {
  x$get(glue2(..., .envir = .envir))
}

source_rrule_js <- function(x) {
  x$source(rrule_js_path)
}

glue2 <- function(..., .envir = parent.frame()) {
  glue(..., .envir = .envir, .open = "[[", .close = "]]")
}
