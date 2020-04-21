new_rcollection <- function(cachers) {
  if (!is_list(cachers)) {
    abort("`cachers` must be a list.")
  }

  map(cachers, validate_cacher)

  cache <- cache_rcollection$new(cachers = cachers)

  data <- list(
    cachers = cachers,
    cache = cache
  )

  new_cacher(data, class = "rcollection")
}
