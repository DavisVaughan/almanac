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
  rschedules <- x$rschedules
  n <- length(rschedules)

  cli::cli_text(cli::format_inline("<rintersect[{n}]>"))

  for (i in seq_len(n)) {
    cli_indented()
    print(rschedules[[i]])
    cli::cli_end()
  }

  invisible(x)
}
