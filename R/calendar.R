# ------------------------------------------------------------------------------

calendar <- function(name,
                     since = "1970-01-01",
                     until = "2100-01-01",
                     adjust_on = c("Saturday", "Sunday")) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  # Adjuster cacher
  adjuster <- weekly(since = since, until = until)
  adjuster <- recur_on_wday(adjuster, adjust_on)

  new_calendar(
    name = name,
    since = since,
    until = until,
    adjuster = adjuster
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
    adjuster = calendar$adjuster,
    holidays = holidays
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name, since, until, adjuster, holidays = list()) {
  if (!is_character(name, n = 1L)) {
    abort("`name` must be a size 1 character vector.")
  }

  if (!is_list(holidays)) {
    abort("`holidays` must be a list of holidays.")
  }

  cachers <- map(
    holidays,
    holiday_initialize,
    since = since,
    until = until,
    adjuster = adjuster
  )

  cache <- cache_rbundle$new(
    cachers = cachers,
    rdates = new_date(),
    exdates = new_date()
  )

  data <- list(
    name = name,
    since = since,
    until = until,
    adjuster = adjuster,
    holidays = holidays,
    cache = cache
  )

  new_cacher(data, class = "calendar")
}

holiday_initialize <- function(holiday, since, until, adjuster) {
  holiday$generator(since, until, adjuster, holiday$adjustment)
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
