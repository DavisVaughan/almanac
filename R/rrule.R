#' @export
daily <- function(since = "1970-01-01") {
  rrule(since, frequency = "daily")
}

#' @export
weekly <- function(since = "1970-01-01") {
  rrule(since, frequency = "weekly")
}

#' @export
monthly <- function(since = "1970-01-01") {
  rrule(since, frequency = "monthly")
}

#' @export
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

# ------------------------------------------------------------------------------

new_rrule <- function(since = as.Date("1970-01-01"),
                      frequency = "yearly",
                      until = NULL,
                      times = NULL,
                      interval = NULL,
                      week_start = NULL,
                      ymonth = NULL,
                      yweek = NULL,
                      yday = NULL,
                      mday = NULL,
                      wday = NULL,
                      position = NULL,
                      easter = NULL) {
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
    position = position,
    easter = easter
  )

  data <- list(
    rules = rules,
    env = env
  )

  structure(data, class = "rrule")
}

# ------------------------------------------------------------------------------

is_rrule <- function(x) {
  inherits(x, "rrule")
}

validate_rrule <- function(x, arg = "`x`") {
  if (!is_rrule(x)) {
    glubort("{arg} must be an rrule.")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

tweak_rrule <- function(x, ...) {
  new <- list2(...)
  old <- x$rules
  old[names(new)] <- new
  exec(new_rrule, !!!old)
}
