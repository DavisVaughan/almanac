vec_cast_date <- function(x, x_arg = "x") {
  if (is.character(x)) {
    vec_cast_date_character(x, x_arg)
  } else {
    vec_cast(x, new_date(), x_arg = x_arg)
  }
}

vec_cast_date_character <- function(x, x_arg) {
  to <- new_date()
  out <- vec_cast(x, to, x_arg = x_arg)
  maybe_lossy_cast(out, x, to, lossy = is.na(out) & !is.na(x))
}

parse_js_date <- function(x) {
  if (length(x) == 0L) {
    return(new_date())
  }

  x <- fast_strptime(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC", lt = FALSE)
  as.Date(x)
}

as_js_from_date <- function(x) {
  x <- as.POSIXlt(x)
  glue("new Date(Date.UTC({year(x)}, {month(x) - 1L}, {day(x)}))")
}

get_rule <- function(x, rule) {
  x[["rules"]][[rule]]
}

is_already_set <- function(x, rule) {
  !is.null(get_rule(x, rule))
}
