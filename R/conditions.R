stop_almanac <- function(message = NULL, class = NULL, ...) {
  abort(message, class = c(class, "almanac_error"), ...)
}

stop_lossy_parse <- function(message) {
  stop_almanac(message = message, class = "almanac_error_lossy_parse")
}

stop_cache_infinite_extension <- function() {
  stop_almanac(
    "Cannot extend the internal cache of occurrences out to an infinite date",
    class = "almanac_error_infinite_extension"
  )
}
