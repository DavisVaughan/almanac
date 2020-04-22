stop_almanac <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "almanac_error"), ...)
}

stop_lossy_parse <- function(message) {
  stop_almanac(message = message, class = "almanac_error_lossy_parse")
}

stop_date_below_minimum <- function(message) {
  stop_almanac(message = message, class = "almanac_error_date_below_minimum")
}

stop_date_above_maximum <- function(message) {
  stop_almanac(message = message, class = "almanac_error_date_above_maximum")
}
