# ------------------------------------------------------------------------------
# Global variables

delayedAssign("almanac_global_context", V8::v8())
delayedAssign("almanac_global_empty_date", vctrs::new_date())
delayedAssign("almanac_global_inf_date", structure(Inf, class = "Date"))
delayedAssign("almanac_global_neg_inf_date", structure(-Inf, class = "Date"))
delayedAssign("almanac_global_na_date", structure(NA_real_, class = "Date"))
delayedAssign("almanac_global_nan_date", structure(NaN, class = "Date"))

delayedAssign("almanac_global_default_since", as.Date("1900-01-01"))
delayedAssign("almanac_global_default_until", as.Date("2100-01-01"))

# JS rrule can't seem to handle dates outside this range, but that's fine
delayedAssign("almanac_global_max_date", as.Date("9999-12-31"))
delayedAssign("almanac_global_min_date", as.Date("0100-01-01"))

# ------------------------------------------------------------------------------

vec_cast_date <- function(x, x_arg = "x") {
  if (is.character(x)) {
    vec_cast_date_from_character(x, x_arg)
  } else {
    vec_cast(x, almanac_global_empty_date, x_arg = x_arg)
  }
}

vec_cast_date_from_character <- function(x, x_arg) {
  # Gives POSIXct with no time component and UTC tz
  out <- lubridate::fast_strptime(x, format = "%Y-%m-%d", tz = "UTC", lt = FALSE)

  # Rely on fast behavior of POSIXct->Date when tz="UTC"
  out <- as.Date.POSIXct(out, tz = "UTC")

  # Check for new `NA` values, these are failed parses
  lossy <- is.na(out) & !is.na(x)

  if (any(lossy)) {
    message <- lossy_to_message(lossy, x_arg)
    stop_lossy_parse(message)
  }

  out
}

lossy_to_message <- function(lossy, x_arg) {
  locations <- which(lossy)
  locations <- as.character(locations)

  if (length(locations) > 1) {
    chr_locations <- "locations"
  } else {
    chr_locations <- "location"
  }

  if (length(locations) > 5) {
    locations <- c(locations[1:5], "etc")
  }

  locations <- glue::glue_collapse(locations, sep = ", ")

  locations

  glue::glue("Failed to parse `{x_arg}` to Date at {chr_locations}: {locations}.")
}

# ------------------------------------------------------------------------------

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

# ------------------------------------------------------------------------------

validate_date_bounds <- function(x, ..., x_arg = "") {
  if (nzchar(x_arg)) {
    x_arg <- glue(" `{x_arg}`")
  }

  if (any(x > almanac_global_max_date, na.rm = TRUE)) {
    date <- format(almanac_global_max_date, format = format_ymd())
    message <- glue("Input{x_arg} cannot be larger than {date}.")
    stop_date_above_maximum(message)
  }

  if (any(x < almanac_global_min_date, na.rm = TRUE)) {
    date <- format(almanac_global_min_date, format = format_ymd())
    message <- glue("Input{x_arg} cannot be smaller than {date}.")
    stop_date_below_minimum(message)
  }

  invisible(x)
}

format_ymd <- function() {
  if (is_linux()) {
    # See `?strptime` section `Printing years`
    "%04Y-%m-%d"
  } else {
    "%Y-%m-%d"
  }
}
is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}

# ------------------------------------------------------------------------------

get_rule <- function(x, rule) {
  x[["rules"]][[rule]]
}

is_already_set <- function(x, rule) {
  !is.null(get_rule(x, rule))
}

# ------------------------------------------------------------------------------

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

weekday_abbr_print <- function() {
  c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
}

# ------------------------------------------------------------------------------

vec_paste0 <- function(...) {
  args <- vec_recycle_common(...)
  exec(paste0, !!!args)
}

# ------------------------------------------------------------------------------

cli_indented <- function(id = NULL, .envir = parent.frame()) {
  cli::cli_div(
    id = id,
    class = "indented",
    theme = list(".indented" = list("margin-left" = 1L)),
    .envir = .envir
  )
}

# ------------------------------------------------------------------------------

is_missing_or_infinite <- function(x) {
  !is.finite(x)
}

is_date <- function(x) {
  inherits(x, "Date")
}
