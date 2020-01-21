# Allows casts of:
# character -> date (with a fix to be lossy on failure)
# date -> date
# (double and integer are too flexible)
# (POSIXct would almost always be lossy)
vec_cast_date <- function(x, x_arg = "x") {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (is.character(x)) {
    out <- vec_cast_date_character(x, x_arg)
    return(out)
  }

  glubort("Can't coerce `{x_arg}` to date. Only character and Date objects are allowed.")
}

vec_cast_date_character <- function(x, x_arg) {
  to <- new_date()
  out <- vec_cast(x, to, x_arg = x_arg)
  maybe_lossy_cast(out, x, to, lossy = is.na(out) & !is.na(x))
}

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

parse_js_date <- function(x) {
  if (length(x) == 0L) {
    return(new_date())
  }

  x <- fast_strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", lt = FALSE)
  as.Date(x)
}

get_rule <- function(x, rule) {
  x[["rules"]][[rule]]
}

is_already_set <- function(x, rule) {
  !is.null(get_rule(x, rule))
}

glue2 <- function(..., .envir = parent.frame()) {
  glue(..., .envir = .envir, .open = "[[", .close = "]]")
}

# ------------------------------------------------------------------------------

wday_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- wday_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    abort("A character `x` must be a weekday name or abbreviation.")
  }

  out <- weekday_int()[where]

  out <- unique(out)

  out
}

wday_match <- function(x) {
  vec_match(x, weekday_name())
}

weekday_name <- function() {
  c(
    c("monday", "mon"),
    c("tuesday", "tues", "tu", "tue"),
    c("wednesday", "wed"),
    c("thursday", "thurs", "thur", "thu", "th"),
    c("friday", "fri"),
    c("saturday", "sat"),
    c("sunday", "sun")
  )
}

# rrule.js wants Monday to be 0, Sunday to be 6.
# We convert to that at the last minute, but otherwise use 1 based integers
weekday_int <- function() {
  c(
    rep(1L, 2L),
    rep(2L, 4L),
    rep(3L, 2L),
    rep(4L, 5L),
    rep(5L, 2L),
    rep(6L, 2L),
    rep(7L, 2L)
  )
}

weekday_print <- function() {
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
}

# ------------------------------------------------------------------------------

month_normalize <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(x)

  where <- month_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    abort("A character `x` must be a month name or abbreviation.")
  }

  out <- month_int()[where]

  out <- unique(out)

  out
}

month_match <- function(x) {
  vec_match(x, month_name())
}

month_name <- function() {
  c(
    tolower(month.name),
    tolower(month.abb),
    "sept" # special case
  )
}

month_int <- function() {
  c(
    1:12,
    1:12,
    9L
  )
}

month_print <- function() {
  month.name
}
