#' @rdname rbundle-set
#' @export
rsetdiff <- function() {
  new_rsetdiff()
}

# ------------------------------------------------------------------------------

#' @rdname new-rbundle-set
#' @export
new_rsetdiff <- function(rschedules = list(),
                         rdates = new_date(),
                         exdates = new_date(),
                         ...,
                         class = character()) {
  cache <- cache_rsetdiff$new(
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
    class = c(class, "rsetdiff")
  )
}

# ------------------------------------------------------------------------------

#' @export
rschedule_events.rsetdiff <- function(x) {
  x$cache$get_events()
}

# ------------------------------------------------------------------------------

#' @export
rbundle_restore.rsetdiff <- function(x, to) {
  new_rsetdiff(
    rschedules = x$rschedules,
    rdates = x$rdates,
    exdates = x$exdates
  )
}

# ------------------------------------------------------------------------------

#' @export
print.rsetdiff <- function(x, ...) {
  print(format(x))
  invisible(x)
}

#' @export
format.rsetdiff <- function(x, ...) {
  n_rschedules <- length(x$rschedules)
  n_rdates <- length(x$rdates)
  n_exdates <-length(x$exdates)

  glue("<rsetdiff[{n_rschedules} rschedules / {n_rdates} rdates / {n_exdates} exdates]>")
}
