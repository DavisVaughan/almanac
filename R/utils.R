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

vec_cast_date <- function(x, ..., x_arg = caller_arg(x), call = caller_env()) {
  if (is.character(x)) {
    vec_cast_date_from_character(x, x_arg = x_arg, call = call)
  } else {
    vec_cast(x, to = almanac_global_empty_date, ..., x_arg = x_arg, call = call)
  }
}

vec_cast_date_from_character <- function(x,
                                         ...,
                                         x_arg = caller_arg(x),
                                         call = caller_env()) {
  # Gives POSIXct with no time component and UTC tz
  out <- lubridate::fast_strptime(x, format = "%Y-%m-%d", tz = "UTC", lt = FALSE)

  # Rely on fast behavior of POSIXct->Date when tz="UTC"
  out <- as.Date.POSIXct(out, tz = "UTC")

  # Check for new `NA` values, these are failed parses
  lossy <- is.na(out) & !is.na(x)

  if (any(lossy)) {
    message <- lossy_to_message(lossy, x_arg)
    stop_lossy_parse(message, call = call)
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

check_date_within_bounds <- function(x,
                                     ...,
                                     arg = caller_arg(x),
                                     call = caller_env()) {
  if (any(x > almanac_global_max_date, na.rm = TRUE)) {
    stop_date_above_maximum(arg = arg, call = call)
  }
  if (any(x < almanac_global_min_date, na.rm = TRUE)) {
    stop_date_below_minimum(arg = arg, call = call)
  }
  invisible(x)
}

check_no_missing <- function(x,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()) {
  missing <- is.na(x)

  if (!any(missing)) {
    return(invisible(NULL))
  }

  missing <- which(missing)

  message <- c(
    "{.arg {arg}} can't contain missing values.",
    i = "Missing values were detected at locations: {missing}."
  )

  cli::cli_abort(message, call = call)
}

check_unique <- function(x,
                         ...,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!vec_duplicate_any(x)) {
    return(invisible(NULL))
  }

  loc <- vec_duplicate_detect(x)
  loc <- which(loc)

  message <- c(
    "{.arg {arg}} can't contain duplicate values.",
    i = "Duplicate values were detected at location: {loc}."
  )

  cli::cli_abort(message, call = call)
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

day_of_week_normalize <- function(x,
                                  ...,
                                  arg = caller_arg(x),
                                  call = caller_env()) {
  if (!is.character(x)) {
    return(x)
  }

  where <- day_of_week_match(x)

  misses <- is.na(where)

  if (any(misses)) {
    cli::cli_abort(
      "A character {.arg {arg}} must be a weekday name or abbreviation.",
      call = call
    )
  }

  out <- day_of_week_int()[where]

  out <- unique(out)

  out
}

day_of_week_match <- function(x) {
  vec_match(tolower(x), day_of_week_names())
}

day_of_week_names <- function() {
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
day_of_week_int <- function() {
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

day_of_week_abbr <- function() {
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

check_inherits <- function(x,
                           what,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (inherits(x, what)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x = x,
    what = cli::format_inline("a <{what}>"),
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# ------------------------------------------------------------------------------

is_missing_or_infinite <- function(x) {
  !is.finite(x)
}

is_date <- function(x) {
  inherits(x, "Date")
}
