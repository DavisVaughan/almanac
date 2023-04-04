stop_almanac <- function(message = NULL, class = NULL, ..., call = caller_env()) {
  abort(message, class = c(class, "almanac_error"), ..., call = call)
}

stop_lossy_parse <- function(message, ..., call = caller_env()) {
  stop_almanac(
    message = message,
    class = "almanac_error_lossy_parse",
    ...,
    call = call
  )
}

stop_date_below_minimum <- function(arg, call) {
  date <- format(almanac_global_min_date, format = format_ymd())

  message <- cli::format_inline(
    "{.arg {arg}} must be larger than {.code {date}}."
  )

  stop_almanac(
    message = message,
    class = "almanac_error_date_below_minimum",
    call = call
  )
}

stop_date_above_maximum <- function(arg, call) {
  date <- format(almanac_global_max_date, format = format_ymd())

  message <- cli::format_inline(
    "{.arg {arg}} must be smaller than {.code {date}}."
  )

  stop_almanac(
    message = message,
    class = "almanac_error_date_above_maximum",
    call = call
  )
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
