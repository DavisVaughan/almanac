stop_almanac <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "almanac_error"), ...)
}

stop_lossy_parse <- function(message) {
  stop_almanac(message = message, class = "almanac_error_lossy_parse")
}
