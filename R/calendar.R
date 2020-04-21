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

  generator <- hldy$generator
  adjustment <- hldy$adjustment

  since <- calendar$since
  until <- calendar$until
  adjuster <- calendar$adjuster

  # Generate the holiday cacher
  cacher <- generator(since, until)

  # Create an adjusted version of it
  cacher <- radjusted(cacher, adjuster, adjustment)

  hldys <- c(calendar$hldys, list(hldy))
  cachers <- c(calendar$cachers, list(cacher))

  new_calendar(
    name = calendar$name,
    since = calendar$since,
    until = calendar$until,
    adjuster = calendar$adjuster,
    hldys = hldys,
    cachers = cachers
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name, since, until, adjuster, hldys = list(), cachers = list()) {
  if (!is_character(name, n = 1L)) {
    abort("`name` must be a size 1 character vector.")
  }

  if (!is_list(hldys)) {
    abort("`hldys` must be a list of holidays.")
  }

  if (!is_list(cachers)) {
    abort("`cachers` must be a list of cachers")
  }

  if (length(cachers) != length(hldys)) {
    abort("`cachers` length must match `hldys` length.")
  }

  # Slightly gross, we get the cache of the rbundle but don't use the
  # rest of it. We don't want to inherit from rbundle, because we don't
  # want to allow `add_cacher()` to work on calendars.
  rbundle <- new_rbundle(cachers = cachers)
  cache <- rbundle$cache

  data <- list(
    name = name,
    since = since,
    until = until,
    adjuster = adjuster,
    hldys = hldys,
    cachers = cachers,
    cache = cache
  )

  new_cacher(data, class = "calendar")
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
