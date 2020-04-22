# ------------------------------------------------------------------------------

calendar <- function(name = NULL,
                     since = "1970-01-01",
                     until = "2040-01-01",
                     adjust_on = c("Saturday", "Sunday")) {
  since <- check_since(since)
  until <- check_until(until)

  if (since > until) {
    abort("`since` must be before `until`.")
  }

  # Adjustment rschedule
  adjustment_rschedule <- weekly(since = since, until = until)
  adjustment_rschedule <- recur_on_wday(adjustment_rschedule, adjust_on)

  new_calendar(
    name = name,
    since = since,
    until = until,
    adjustment_rschedule = adjustment_rschedule
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.calendar <- function(x) {
  rschedule_events(x$rbundle)
}

# ------------------------------------------------------------------------------

add_hldy <- function(x, hldy) {
  validate_calendar(x)
  validate_hldy(hldy)

  if (hldy_exists(hldy, x)) {
    warn("`hldy` already exists in the calendar, returning calendar unmodified.")
    return(x)
  }

  generator <- hldy$generator
  adjustment <- hldy$adjustment

  since <- x$since
  until <- x$until
  adjustment_rschedule <- x$adjustment_rschedule

  # Generate the holiday rschedule
  rschedule <- generator(since, until)

  # Create an adjusted version of it
  rschedule <- radjusted(rschedule, adjustment_rschedule, adjustment)

  hldys <- c(x$hldys, list(hldy))
  rschedules <- c(x$rschedules, list(rschedule))

  new_calendar(
    name = x$name,
    since = x$since,
    until = x$until,
    adjustment_rschedule = x$adjustment_rschedule,
    hldys = hldys,
    rschedules = rschedules
  )
}

# ------------------------------------------------------------------------------

# Remove by name or by object that has that name
remove_hldy <- function(x, hldy) {
  validate_calendar(x, x_arg = "x")

  if (is_hldy(hldy)) {
    hldy <- hldy_name(hldy)
  }
  if (!is_string(hldy)) {
    abort("`hldy` must be a single character name or a hldy object.")
  }

  names <- calendar_names(x)

  indicator <- vec_in(hldy, names)
  name_exists <- any(indicator)

  # Early return if name didn't exist
  if (!name_exists) {
    return(x)
  }

  keep <- !indicator

  hldys <- x$hldys
  rschedules <- x$rschedules

  hldys <- hldys[keep]
  rschedules <- rschedules[keep]

  new_calendar(
    name = x$name,
    since = x$since,
    until = x$until,
    adjustment_rschedule = x$adjustment_rschedule,
    hldys = hldys,
    rschedules = rschedules
  )
}

# ------------------------------------------------------------------------------

new_calendar <- function(name,
                         since,
                         until,
                         adjustment_rschedule,
                         hldys = list(),
                         rschedules = list()) {
  if (!(is.null(name) || is_character(name, n = 1L))) {
    abort("`name` must be a size 1 character vector or `NULL`.")
  }

  if (!is_list(hldys)) {
    abort("`hldys` must be a list of holidays.")
  }

  if (!is_list(rschedules)) {
    abort("`rschedules` must be a list of rschedules")
  }

  if (length(rschedules) != length(hldys)) {
    abort("`rschedules` length must match `hldys` length.")
  }

  rbundle <- new_rbundle(rschedules = rschedules)

  data <- list(
    name = name,
    since = since,
    until = until,
    adjustment_rschedule = adjustment_rschedule,
    hldys = hldys,
    rschedules = rschedules,
    rbundle = rbundle
  )

  new_rschedule(data, class = "calendar")
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

# ------------------------------------------------------------------------------

hldy_exists <- function(hldy, calendar) {
  names <- calendar_names(calendar)
  hldy_name(hldy) %in% names
}

calendar_names <- function(x) {
  map_chr(x$hldys, hldy_name)
}
