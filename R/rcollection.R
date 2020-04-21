rcollection <- function() {
  new_rcollection()
}

rc_add_cacher <- function(x, cacher) {
  validate_cacher(cacher, "cacher")

  cachers <- c(x$cachers, list(cacher))

  new_rcollection(
    cachers = cachers,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

rc_add_rdates <- function(x, rdates) {
  validate_cacher(cacher, "cacher")
  rdates <- vec_cast_date(rdates, "rdates")

  validate_date_bounds(rdates, x_arg = "rdates")

  rdates <- vec_c(x$rdates, rdates)
  rdates <- vec_unique(rdates)

  new_rcollection(
    cachers = x$cachers,
    rdates = rdates,
    exdates = x$exdates
  )
}

rc_add_exdates <- function(x, exdates) {
  validate_cacher(cacher, "cacher")
  exdates <- vec_cast_date(exdates, "exdates")

  validate_date_bounds(exdates, x_arg = "exdates")

  exdates <- vec_c(x$exdates, exdates)
  exdates <- vec_unique(exdates)

  new_rcollection(
    cachers = x$cachers,
    rdates = x$rdates,
    exdates = exdates
  )
}

new_rcollection <- function(cachers = list(),
                            rdates = new_date(),
                            exdates = new_date()) {
  if (!is_list(cachers)) {
    abort("`cachers` must be a list.")
  }

  if (!is_date(rdates)) {
    abort("`rdates` must inherit from Date.")
  }

  if (!is_date(exdates)) {
    abort("`exdates` must inherit from Date.")
  }

  cache <- cache_rcollection$new(
    cachers = cachers,
    rdates = rdates,
    exdates = exdates
  )

  data <- list(
    cachers = cachers,
    rdates = rdates,
    exdates = exdates,
    cache = cache
  )

  new_cacher(data, class = "rcollection")
}

