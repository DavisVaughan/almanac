roffset <- function(rschedule, offset) {
  check_number_whole(offset)
  offset <- vec_cast(offset, to = integer())
  new_roffset(rschedule = rschedule, offset = offset)
}

new_roffset <- function(rschedule, offset) {
  validate_rschedule(rschedule, "rschedule")

  if (!is_integer(offset)) {
    abort("`offset` must be an integer.")
  }

  cache <- cache_roffset$new(
    rschedule = rschedule,
    offset = offset
  )

  new_rschedule(
    rschedule = rschedule,
    offset = offset,
    cache = cache,
    class = "roffset"
  )
}

# ------------------------------------------------------------------------------

#' @export
print.roffset <- function(x, ...) {
  cat_line(glue("<roffset[offset: {x$offset}]>"))
  print(x$rschedule)
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.roffset <- function(x) {
  x$cache$get_events()
}
