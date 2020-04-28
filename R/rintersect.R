#' @rdname rbundle-set
#' @export
rintersect <- function() {
  new_rintersect()
}

# ------------------------------------------------------------------------------

#' @rdname new-rbundle-set
#' @export
new_rintersect <- function(rschedules = list(),
                           rdates = new_date(),
                           exdates = new_date(),
                           ...,
                           class = character()) {
  cache <- cache_rintersect$new(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates
  )

  new_rbundle(
    rschedules = rschedules,
    rdates = rdates,
    exdates = exdates,
    cache = cache,
    ...,
    class = c(class, "rintersect")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rintersect <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
rbundle_restore.rintersect <- function(x, to) {
  new_rintersect(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

#' @export
print.rintersect <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.rintersect <- function(x, ...) {
  n_rschedules <- length(x$rschedules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("<rintersect[{n_rschedules} rschedules / {n_rdates} rdates / {n_exdates} exdates]>")
}
