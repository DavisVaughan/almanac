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

stop_date_below_minimum <- function(message, ..., call = caller_env()) {
  stop_almanac(
    message = message,
    class = "almanac_error_date_below_minimum",
    ...,
    call = call
  )
}

stop_date_above_maximum <- function(message, ..., call = caller_env()) {
  stop_almanac(
    message = message,
    class = "almanac_error_date_above_maximum",
    ...,
    call = call
  )
}
