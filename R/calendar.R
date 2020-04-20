# ------------------------------------------------------------------------------

calendar <- function(name,
                     since = "1970-01-01",
                     until = "2100-01-01",
                     weekend = c("Saturday", "Sunday")) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  # Weekend rbundle
  rrule <- weekly(since = since, until = until)
  rrule <- recur_on_wday(rrule, weekend)
  weekend <- rbundle()
  weekend <- add_rrule(weekend, rrule)

  new_calendar(
    name = name,
    since = since,
    until = until,
    weekend = weekend
  )
}

# ------------------------------------------------------------------------------

add_hldy <- function(calendar, hldy) {
  validate_calendar(calendar)
  validate_hldy(hldy)

  holidays <- c(calendar$holidays, list(hldy))

  new_calendar(
    name = calendar$name,
    since = calendar$since,
    until = calendar$until,
    weekend = calendar$weekend,
    holidays = holidays
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name, since, until, weekend, holidays = list()) {
  if (!is_character(name, n = 1L)) {
    abort("`name` must be a size 1 character vector.")
  }

  if (!is_list(holidays)) {
    abort("`holidays` must be a list of holidays.")
  }

  rbundles <- map(
    holidays,
    holiday_initialize,
    since = since,
    until = until
  )

  adjustments <- map(
    holidays,
    holiday_adjustment
  )

  cache <- cache_calendar$new(
    rbundles = rbundles,
    adjustments = adjustments,
    weekend = weekend
  )

  data <- list(
    name = name,
    since = since,
    until = until,
    weekend = weekend,
    holidays = holidays,
    cache = cache
  )

  new_cacher(data, class = "calendar")
}

holiday_initialize <- function(holiday, since, until) {
  holiday$generator(since, until)
}

holiday_adjustment <- function(holiday) {
  holiday$adjustment
}

# ------------------------------------------------------------------------------

is_calendar <- function(x) {
  inherits(x, "calendar")
}

validate_calendar <- function(x, x_arg = "calendar") {
  if (!is_calendar(x)) {
    glubort("`{x_arg}` must be a calendar.")
  }
  invisible(x)
}
