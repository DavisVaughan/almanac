#' @rdname rbundle-set
#' @export
runion <- function() {
  new_runion()
}

# ------------------------------------------------------------------------------

#' @rdname new-rbundle-set
#' @export
new_runion <- function(rschedules = list(),
                       rdates = new_date(),
                       exdates = new_date(),
                       ...,
                       class = character()) {
  cache <- cache_runion$new(
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
    class = c(class, "runion")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.runion <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
rbundle_restore.runion <- function(x, to) {
  new_runion(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

#' @export
print.runion <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.runion <- function(x, ...) {
  n_rschedules <- length(x$rschedules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("<runion[{n_rschedules} rschedules / {n_rdates} rdates / {n_exdates} exdates]>")
}
