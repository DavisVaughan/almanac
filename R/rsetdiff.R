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
  rschedules <- x$rschedules
  n <- length(rschedules)

  cli::cli_text(cli::format_inline("<rsetdiff[{n}]>"))

  for (i in seq_len(n)) {
    cli_indented()
    print(rschedules[[i]])
    cli::cli_end()
  }

  invisible(x)
}
